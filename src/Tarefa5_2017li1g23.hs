{-|
Module      : Tarefa5_2017li1g23
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where

import LI11718
import Tarefa3_2017li1g23
import Tarefa4_2017li1g23
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture

{-| Tipo que inclui as imagens necessárias ao jogo
-}
data Imagens = Imagens
  { car  :: Picture
  , magma :: [Picture]
  , menu :: Picture
  }
  deriving (Show)

type GameState = (Jogo,[Acao],Imagens,Int,Int,(Float,Float))

{-| Figura
-}
roadi =  Color (greyN 0.6) (rectangleSolid 50 50)
{-| Figura
-}
curvei = Color (greyN 0.6) (Polygon [(-25,-25),(25,-25),(25,25),(-25,-25)])
{-| Figura
-}
rampi = Pictures [Color (greyN 0.3) (rectangleSolid 50 50),Color (greyN 0.6) (Polygon [(0,25),(25,-25),(-25,-25),(0,25)])]
{-| Figura
-}
instructions = Scale 0.5 0.5 (Color white (Pictures [Translate (-100) 150 (Scale 0.35 0.35 (Text "Movement:")),Line [(-25,50),(0,100),(25,50)],Line [(-25,25),(0,-25),(25,25)],Line [(-50,-25),(-100,0),(-50,25)],Line [(50,-25),(100,0),(50,25)],Translate (-115) (-200) (Scale 0.35 0.35 (Text "Nitro: Shift"))]))

{-| Função dá a o estado incial de todos os os aspetos relacionados com a execução do programa
-}
estadoInicial :: Imagens -> GameState
estadoInicial i = (jogoInit,acaoInicial,i,0,0,(0,20000))

{-| Função dá o jogo inicial
-}
jogoInit :: Jogo
jogoInit = Jogo {mapa = daMap, pista = Propriedades {k_atrito = 1, k_pneus = 1, k_acel = 2, k_peso = 1.5, k_nitro = 4, k_roda = 180},carros = [Carro {posicao = (5.5,3.5), direcao = 0, velocidade = (0,0)}],nitros = [4],historico = [[]]}

{-| Função dá a ação incial do jogador
-}
acaoInicial :: [Acao]
acaoInicial = [Acao {acelerar = False,travar = False,esquerda = False,direita = False,nitro = Nothing}]

{-| Função que desenha cada iteração do estado na janela
-}
desenhaEstado :: GameState -> Picture
desenhaEstado (j,a,Imagens {car = carro, magma = lava, menu = men},0,t,time) = Pictures [men,Translate 132 (-125) (lava!!t)]
desenhaEstado (Jogo {mapa = Mapa ini tab, pista = prop,carros = ((Carro {posicao = (x,y), direcao = d, velocidade = (vy,vx)}):c),nitros = ln,historico = lh}, action,Imagens {car = carro, magma = lava, menu = men},1,t,time) = Pictures (((Translate (-300) 100 (instructions)):(scale (0.5) (0.5) (Translate (250) (0) (drawTimer time))):(drawTab tab 0 0 (((fst (dimTab tab))-1)/2,((snd (dimTab tab))-1)/2) (Imagens {car = carro, magma = lava,menu = men}) t))++[Translate (((double2Float (x))-((fst (dimTab tab))/2))*(50)) (((double2Float (y))-((snd (dimTab tab))/2))*(-50)) (Rotate (double2Float (-d)) carro)])
desenhaEstado (j,a,i,2,t,(time,record)) = Pictures [Color yellow (rectangleSolid 3000 3000),(Scale (0.15) (0.15) (Translate (-1400) (0) (Text ("O teu tempo foi "++(show ((fromInteger $ round $ time * (10^2)) / (10.0^^2)))++" e o teu recorde é"++(show ((fromInteger $ round $ record * (10^2)) / (10.0^^2))))))),(Translate (-185) 100 (Text "Victory")),Scale (0.25) (0.25) (Translate (-1400) (-950) (Text "Click na janela para voltar ao menu inicial"))]

{-| Função que descobre a dimensão de um tabuleiro
-}
dimTab :: Tabuleiro -> (Float,Float)
dimTab t = ((fromIntegral (length (head t))),(fromIntegral (length t)))

{-| Função que desenha o contador do tempo
-}
drawTimer :: (Float,Float) -> Picture
drawTimer (time,record) | record > 15000 = Pictures ((Color white (Scale (0.3) (0.3) (Translate 700 600 (Text ("Timer: "++(show ((fromInteger $ round $ time * (10^2)) / (10.0^^2)))))))):[Color white (Scale (0.3) (0.3) (Translate 700 200 (Text ("Record: -"))))])
                        | otherwise = Pictures ((Color white (Scale (0.3) (0.3) (Translate 700 600 (Text ("Timer: "++(show ((fromInteger $ round $ time * (10^2)) / (10.0^^2)))))))):[Color white (Scale (0.3) (0.3) (Translate 700 200 (Text ("Record: "++(show ((fromInteger $ round $ record * (10^2)) / (10.0^^2)))))))])

{-| Função que desenha o tabuleiro passado ao mapa dando uma lista de pictures correspondentes as pecas no tabuleiro
-}
drawTab :: Tabuleiro -> Float -> Float -> (Float,Float) -> Imagens -> Int -> [Picture]
drawTab [] l a m i tem = []
drawTab ([]:t) l a m i tem = drawTab t 0 (a+1) m i tem
drawTab (((Peca p h):ps):t) l a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem | p == Lava = (Translate (-(sl-l)*50) ((sa-a)*50) (lava!!tem)):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Recta = (Translate (-(sl-l)*50) ((sa-a)*50) roadi):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Rampa Sul = (Translate (-(sl-l)*50) ((sa-a)*50) rampi):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Rampa Norte = (Translate (-(sl-l)*50) ((sa-a)*50) (Rotate 180 rampi)):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Rampa Este = (Translate (-(sl-l)*50) ((sa-a)*50) (Rotate 270 rampi)):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Rampa Oeste = (Translate (-(sl-l)*50) ((sa-a)*50) (Rotate 90 rampi)):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Curva Sul = (Translate (-(sl-l)*50) ((sa-a)*50) (lava!!tem)):(Translate (-(sl-l)*50) ((sa-a)*50) (Rotate 180 curvei)):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Curva Norte = (Translate (-(sl-l)*50) ((sa-a)*50) (lava!!tem)):(Translate (-(sl-l)*50) ((sa-a)*50) curvei):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Curva Este = (Translate (-(sl-l)*50) ((sa-a)*50) (lava!!tem)):(Translate (-(sl-l)*50) ((sa-a)*50) (Rotate 90 curvei)):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
                                                                                       | p == Curva Oeste = (Translate (-(sl-l)*50) ((sa-a)*50) (lava!!tem)):(Translate (-(sl-l)*50) ((sa-a)*50) (Rotate 270 curvei)):(drawTab (ps:t) (l+1) a (sl,sa) (Imagens {car = c, magma = lava,menu = m}) tem)
--
{-| Função que trata de todas as reações no caso de uma ação pelo jogador
-}
reageEvento :: Event -> GameState -> GameState
reageEvento (EventKey (SpecialKey KeyUp) key _ _) s = moveUp key s
reageEvento (EventKey (SpecialKey KeyDown) key _ _) s = moveDown key s
reageEvento (EventKey (SpecialKey KeyRight) key _ _) s = moveRight key s
reageEvento (EventKey (SpecialKey KeyLeft) key _ _) s = moveLeft key s
reageEvento (EventKey (SpecialKey KeyShiftL) key _ _) s = shiftNitro key s
reageEvento (EventKey (MouseButton LeftButton) Up _ (x,y)) s = clickMenu (x,y) s
reageEvento _ s = s

{-| Função que trata de todos as ações a tomar no caso de um clique na seta para cima pelo jogador
-}
moveUp :: KeyState -> GameState -> GameState
moveUp k (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time) | k == Down && est == 1 = (j,(Acao {acelerar = True,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,1,tem,time)
                                                                                                     | k == Up && est == 1 = (j,(Acao {acelerar = False,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,1,tem,time)
                                                                                                     | otherwise = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time)

{-| Função que trata de todos as ações a tomar no caso de um clique na seta para baixo pelo jogador
-}
moveDown :: KeyState -> GameState -> GameState
moveDown k (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time) | k == Down && est == 1 = (j,(Acao {acelerar = a,travar = True,esquerda = e,direita = d,nitro = n}:t),i,1,tem,time)
                                                                                                       | k == Up && est == 1 = (j,(Acao {acelerar = a,travar = False,esquerda = e,direita = d,nitro = n}:t),i,1,tem,time)
                                                                                                       | otherwise = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time)

{-| Função que trata de todos as ações a tomar no caso de um clique na seta direita pelo jogador
-}
moveRight :: KeyState -> GameState -> GameState
moveRight k (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time) | k == Down && est == 1 = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = True,nitro = n}:t),i,1,tem,time)
                                                                                                        | k == Up && est == 1 = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = False,nitro = n}:t),i,1,tem,time)
                                                                                                        | otherwise = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time)

{-| Função que trata de todos as ações a tomar no caso de um clique na seta esquerda pelo jogador
-}
moveLeft :: KeyState -> GameState -> GameState
moveLeft k (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time) | k == Down && est == 1 = (j,(Acao {acelerar = a,travar = trav,esquerda = True,direita = d,nitro = n}:t),i,1,tem,time)
                                                                                                       | k == Up && est == 1 = (j,(Acao {acelerar = a,travar = trav,esquerda = False,direita = d,nitro = n}:t),i,1,tem,time)
                                                                                                       | otherwise = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time)

{-| Função que trata de todos as ações a tomar no caso de um cliqueno shift pelo jogador
-}
shiftNitro :: KeyState -> GameState -> GameState
shiftNitro k (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time) | k == Down && est == 1 = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = Just 0}:t),i,1,tem,time)
                                                                                                         | k == Up && est == 1 = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = Nothing}:t),i,1,tem,time)
                                                                                                         | otherwise = (j,(Acao {acelerar = a,travar = trav,esquerda = e,direita = d,nitro = n}:t),i,est,tem,time)

{-| Função que trata de todos as ações a tomar no caso de um clique do rato pelo jogador
-}
clickMenu :: (Float,Float) -> GameState -> GameState
clickMenu (x,y) (j,a,i,0,t,(tempo,rec)) | x>(-120) && x<120 && y<40 && y>(-50) =  (j,a,i,1,t,(0,rec))
                                        | x>(-220) && x<170 && y<(-90) && y>(-170) =  (j,a,i,0,temaCiclo t,(tempo,rec))
                                        | otherwise = (j,a,i,0,t,(tempo,rec))
clickMenu (x,y) (j,a,i,2,t,time) = (j,a,i,0,t,time)
clickMenu (x,y) (j,a,i,e,t,time) = (j,a,i,e,t,time)

{-| Função que ao chegar ao ultimo tema volta ao primeiro
-}
temaCiclo :: Int -> Int
temaCiclo i | (i + 1) > 2 = 0
            | otherwise = i + 1

{-| Função que corre o jogo de iteração em iteração
-}
reageTempo :: Float -> GameState -> GameState
reageTempo n (j,a,i,0,t,time) = (j,a,i,0,t,time)
reageTempo n (Jogo {mapa = m, pista = p,carros = lc,nitros = ln,historico = lh}, a, i,1,t,(time,record)) | not (fst (completeRace m lh 0)) = (actCar (float2Double n) (actGame (float2Double n) (Jogo {mapa = m, pista = p,carros = lc,nitros = ln,historico = lh}) ((length lc) - 1) a), a,i,1,t,(time+(1/30),record))
                                                                                                         | otherwise = (Jogo {mapa = m, pista = p,carros = lc,nitros = ln,historico = lh},a,i,2,t,isRecord (time,record))
reageTempo n (j,a,i,2,t,time) = (jogoInit,acaoInicial,i,2,t,time)

{-| Função que verifica se o novo tempo é um recorde da sessão
-}
isRecord :: (Float,Float) -> (Float,Float)
isRecord (t,r) | t < r = (t,t)
               | otherwise = (t,r)

{-| Função que verifica se algum jogador já passou todas as peças do precurso, e se sim qual deles
-}
completeRace :: Mapa -> [[Posicao]] -> Int -> (Bool,Int)
completeRace m [] j = (False,0)
completeRace (Mapa p t) (h:hs) j | (length h) == (length (nPecasNoTabuleiro t)) = (True,j+1)
                                 | otherwise = completeRace (Mapa p t) hs (j+1)

{-| Função que verifica qual é o número de peças num tabuleiro
-}
nPecasNoTabuleiro :: Tabuleiro -> [Int]
nPecasNoTabuleiro [] = []
nPecasNoTabuleiro (h:t) = (nPecasNoTabuleiroLinha h)++(nPecasNoTabuleiro t)
{-| Função que verifica qual é o número de peças numa linha
-}
nPecasNoTabuleiroLinha :: [Peca] -> [Int]
nPecasNoTabuleiroLinha [] = []
nPecasNoTabuleiroLinha (h:t) | h==(Peca Lava 0) = nPecasNoTabuleiroLinha t
                             | otherwise = 1:(nPecasNoTabuleiroLinha t)

{-| Função que atualiza todos os jogadores de acordo com as suas ações usando a função atualiza da tarefa 4
-}
actGame :: Tempo -> Jogo -> Int -> [Acao] -> Jogo
actGame t e 0 [a] = atualiza t e 0 a
actGame t e j (a:as) = actGame t (atualiza t e j a) (j-1) as

{-| Função que atualiza a posição de todos os carros usando a função movimenta  da tarefa 3 após a função actGame
-}
actCar :: Tempo -> Jogo -> Jogo
actCar t (Jogo {mapa = Mapa ini tab, pista = p,carros = lc,nitros = ln,historico = lh}) = Jogo {mapa = Mapa ini tab, pista = p,carros = justCarro (map (movimenta tab t) lc) lh,nitros = ln,historico = lh}

{-| Função que transforma o resultado da função movimenta do tipo Maybe Carro no tipo Carro
-}
justCarro :: [Maybe Carro] -> [[Posicao]] -> [Carro]
justCarro [] m = []
justCarro ((Just car):mcs) (h:t) = car:(justCarro mcs t)
justCarro (Nothing:mcs) (h:t) = (Carro {posicao = ((fromIntegral (fst (head h)))+0.5,(fromIntegral (snd (head h)))+0.5), direcao = 0, velocidade = (0,0)}):(justCarro mcs t)

{-| Função dá o 1/framerate do jogo
-}
fr :: Int
fr = 30

{-| Função dá o a janela do jogo
-}
dm :: Display
dm = InWindow "Novo Jogo" (800,600) (0,0)

{-|
Função que dá o mapa a usar no jogo.
-}
daMap :: Mapa
daMap = Mapa ((5,3),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca (Curva Este) 1,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-|
Função principal usada para animar um jogo completo.
Compilar com o GHC.
-}

main :: IO ()
main = do c <- loadBMP "car.bmp"
          l <- loadBMP "lava.bmp"
          l2 <- loadBMP "lava2.bmp"
          l3 <- loadBMP "lava3.bmp"
          m <- loadBMP "menu.bmp"
          play dm
               black
               fr
               (estadoInicial (Imagens {car = c,magma = [l,l2,l3],menu = m}))
               desenhaEstado
               reageEvento
               reageTempo
