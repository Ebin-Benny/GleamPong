module Main(main) where

import Gleam

type Radius = Double
type Position = (Double, Double)
data PlayerMovement = PlayerUp | PlayerStill | PlayerDown deriving Show
data Winner = Player1 | Player2 | NoOne

data PongGame = Game
    { ballLoc :: Position
    , ballDirection :: (Double, Double)
    , ballSpeed :: Double
    , player1 :: Position
    , player1Movement :: PlayerMovement
    , player2 :: Position
    , player2Movement :: PlayerMovement
    , paused :: Bool
    } deriving Show

playerSpeed = 4.0    
ballRadius = 10
paddleWidth = 13
paddleLength = 36
wallHeight = 10
wallWidth = 400

paddleYMax = 200 - paddleLength - wallHeight

initialPosition = (0.0, 0.0)
initialDirection = (1.5, 1.0)
initialSpeed = 1.0
speedIncrement = 0.1

ballColor = White

wallColor = Rose

windowWidth = 400;
windowHeight = 400;


main :: IO ()
main = play (GleamConfig windowWidth windowHeight) initialState draw update handleKeys

update :: PongGame -> PongGame
update game = if (paused game)
              then game
              else (moveBall . wallBounce . paddleBounce . movePlayers . handleBallOutOfBounds) game

initialState :: PongGame
initialState = Game initialPosition initialDirection initialSpeed
    (-175.0, 20.0) PlayerStill
    (175.0, 100.0) PlayerStill
    False

drawBackground :: Picture
drawBackground = Color Blue $ Rectangle (fromIntegral windowWidth) (fromIntegral windowHeight)

drawPaddle :: Color -> Position -> Picture
drawPaddle col pos = Pictures
    [ Translate x y $ Color col $ Rectangle (2 * paddleWidth) (2 * paddleLength)]
    where (x, y) = pos

drawBall :: PongGame -> Picture
drawBall game = uncurry Translate (ballLoc game) $ Color ballColor $ Circle ballRadius

drawWall :: Double -> Picture
drawWall offset =
    Translate 0 offset $ Color wallColor $ Rectangle wallWidth wallHeight

drawWalls :: Picture
drawWalls = Pictures [drawWall 200, drawWall (-200)]

draw :: PongGame -> Picture
draw game = Pictures [ drawBackground, drawBall game, drawWalls, drawPaddle White (player1 game), drawPaddle Orange (player2 game)]

moveBall :: PongGame -> PongGame
moveBall game = game { ballLoc = (x', y') }
    where
    (x, y) = ballLoc game
    (vx, vy) = ballDirection game
    speed = ballSpeed game
    x' = x + vx * speed
    y' = y + vy * speed

checkWallCollision :: Position -> Bool
checkWallCollision (_, y) = topCollision || bottomCollision
    where
    topCollision = y - ballRadius <= -fromIntegral windowWidth / 2
    bottomCollision = y + ballRadius >= fromIntegral windowWidth / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballDirection = (vx, vy') }
    where
    (vx, vy) = ballDirection game
    vy' = if checkWallCollision (ballLoc game) then -vy else vy


checkLeftPaddleCollision :: Position -> Position -> Bool
checkLeftPaddleCollision ballPosition player = xCollision && yCollision
    where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = (x - ballRadius <= px + paddleWidth) && (x - ballRadius >= px - paddleWidth)
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)

checkRightPaddleCollision :: Position -> Position -> Bool
checkRightPaddleCollision ballPosition player = xCollision && yCollision
    where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = (x + ballRadius >= px - paddleWidth) && (x + ballRadius <= px + paddleWidth)
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballDirection = (vx', vy), ballSpeed = speed' }
    where
    (vx, vy) = ballDirection game
    leftCollision = checkLeftPaddleCollision (ballLoc game) (player1 game)
    rightCollision = checkRightPaddleCollision (ballLoc game) (player2 game)
    vx' = if leftCollision then abs vx
            else if rightCollision then - (abs vx)
            else vx
    speed' = if leftCollision || rightCollision
                then (ballSpeed game) + speedIncrement
                else (ballSpeed game)

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

lockPaddleBounds :: Position -> Position
lockPaddleBounds (px, py) = (px, clamp (-paddleYMax) paddleYMax py)

movePlayer :: Position -> PlayerMovement -> Position
movePlayer (px, py) PlayerUp = (px, py - playerSpeed)
movePlayer (px, py) PlayerStill = (px, py)
movePlayer (px, py) PlayerDown = (px, py + playerSpeed)

movePlayers :: PongGame -> PongGame
movePlayers game = game { player1 = lockPaddleBounds $ movePlayer (player1 game) (player1Movement game)
                        , player2 = lockPaddleBounds $ movePlayer (player2 game) (player2Movement game)
                        }

winner :: Position -> Winner
winner (bx, _) | bx >= 200 = Player1
winner (bx, _) | bx <= (-200) = Player2
winner _ = NoOne

handleBallOutOfBounds :: PongGame -> PongGame
handleBallOutOfBounds game = case winner (ballLoc game) of
                                NoOne -> game
                                Player1 -> game { ballLoc = initialPosition, ballSpeed = initialSpeed }
                                Player2 -> game { ballLoc = initialPosition, ballSpeed = initialSpeed }


handleKeys :: InputEvent -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') Down) game = game { ballLoc = initialPosition }
handleKeys (EventKey (Char 'p') Down) game = game { paused = not (paused game) }

handleKeys (EventKey (Char 'w') Down) game = game { player1Movement = PlayerUp }
handleKeys (EventKey (Char 'w') Up) game = game { player1Movement = PlayerStill }
handleKeys (EventKey (Char 's') Down) game = game { player1Movement = PlayerDown }
handleKeys (EventKey (Char 's') Up) game = game { player1Movement = PlayerStill }

handleKeys (EventKey (Char 'i') Down) game = game { player2Movement = PlayerUp }
handleKeys (EventKey (Char 'i') Up) game = game { player2Movement = PlayerStill }
handleKeys (EventKey (Char 'k') Down) game = game { player2Movement = PlayerDown }
handleKeys (EventKey (Char 'k') Up) game = game { player2Movement = PlayerStill }
    
handleKeys _ game = game