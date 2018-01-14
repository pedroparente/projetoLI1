{-|
Module : Tarefa2_2017li1g23
Description : Módulo correspondente à 3ª Tarefa do projeto de LI1
Copyright : Nuno Cunha <a85400@alunos.uminho.pt>
            Pedro Parente <a85919alunos.uminho.pt>

Módulo em que foi executada a terceira tarefa do projeto de LI1 com o objetivo de calcular o "estado" de um carro, em qualquer momento fornecido
-}
module Tarefa3_2017li1g23 where

import LI11718

{-| Armazena todos os testes a correr para testar o estado do carro após o tempo fornecido
-}
testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [movRetasCurva,movRetasCurvaDestroi,movRampasSobe,movRampasSobeDestroi,movRampasDesce,movRampasDesceCai,movColideRicochete,movPassaPorCurva,movAcabaEmCurva,movAcabaEmLavaDaCurva,movcolide2vezes]
{-| A Função movimenta, dado um tabuleiro, um tempo e o estado inicial de um carro (posição, direção e velocidade), calcula o novo estado desse mesmo carro, de acordo com as regras fornecidas

==Exemplos de Utilização
>>> movimenta tabtestes2 5 (Carro {posicao = (2.3,1.9), direcao = 0, velocidade = (1,(-0.1))})
Just (Carro {posicao = (7.3,1.4000000000000001), direcao = 45.0, velocidade = (1.0,-0.1)})
-}
movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta m t (Carro {posicao = (posx,posy), direcao = ang, velocidade = (0,0)}) = Just (Carro {posicao = (posx,posy), direcao = ang, velocidade = (0,0)})
movimenta m t (Carro {posicao = (posx,posy), direcao = ang, velocidade = (vx,vy)}) = movimentaAux (pecaAtual m (posx,posy)) m t (Carro {posicao = (posx,posy), direcao = ang, velocidade = (vx,vy)})
{-| função auxiliar da movimenta
-}
movimentaAux :: Peca -> Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimentaAux peca m t (Carro {posicao = (posx,posy), direcao = ang, velocidade = (vx,vy)}) | novoTempo <= 0 && naLava == True = Nothing
                                                                                           | novoTempo <= 0 && naLava == False = Just Carro {posicao = (posx + (vx * t),posy + (vy * t)), direcao = ang, velocidade = (vx,vy)}
                                                                                           | caiNaLava m peca p v (proximaPeca m v (posx,posy)) || caiNivel peca (proximaPeca m v (posx,posy)) = Nothing
                                                                                           | colideVert peca (proximaPeca m v (posx,posy)) v = movimentaAux (pecaAtualLimite m v p) m novoTempo (justTo (Just Carro {posicao = p, direcao = ang, velocidade = (vx,(-vy))}))
                                                                                           | colideHori peca (proximaPeca m v (posx,posy)) v = movimentaAux (pecaAtualLimite m v p) m novoTempo (justTo (Just Carro {posicao = p, direcao = ang, velocidade = ((-vx),vy)}))
                                                                                           | otherwise = movimentaAux (pecaAtualLimite m v p) m novoTempo (justTo (Just Carro {posicao = p, direcao = ang, velocidade = (vx,vy)}))
    where (d,p,v) = retaIntersect (posx,posy) (vx,vy)
          novoTempo = retiraTempo t (posx,posy) (vx,vy)
          naLava = inLava m (posx + (vx * t),posy + (vy * t)) (pecaAtualLimite m v p)
{-| Função que calcula a intercessão de duas retas
-}
maybeIntersection :: (Ponto,Ponto) -> (Ponto,Ponto) -> Maybe Ponto
maybeIntersection ((ax, ay), (bx, by)) ((px, py), (qx, qy)) =
  let (pqDX, abDX) = (px - qx, ax - bx)
      (pqDY, abDY) = (py - qy, ay - by)
      determinant = abDX * pqDY - abDY * pqDX
      f pq ab =
        ((((ax * by) - (ay * bx)) * pq) -
         (((px * qy) - (py * qx)) * ab)) /
        determinant
  in case determinant of
       0 -> Nothing
       _ -> Just (f pqDX abDX, f pqDY abDY)

{-| Funcção que transforma um tipo Maybe a em a
-}
justTo :: Maybe a -> a
justTo (Just a) = a

{-| Função que verifica qual a reta que vai ser intercetada à esquerda
-}
retaxEsq :: Ponto -> Int
retaxEsq (posx,posy) | fromIntegral (floor posx) == posx = (floor posx) - 1
                     | otherwise = floor posx
{-| Função que verifica qual a reta que vai ser intercetada à esquerda
-}
retaxDir :: Ponto -> Int
retaxDir (posx,posy) | fromIntegral (ceiling posx) == posx = (ceiling posx) + 1
                     | otherwise = ceiling posx
{-| Função que verifica qual a reta que vai ser intercetada em cima
-}
retaycima :: Ponto -> Int
retaycima (posx,posy) | fromIntegral (floor posy) == posy = (floor posy) - 1
                      | otherwise = floor posy
{-| Função que verifica qual a retaque vai ser intercetada em baixo
-}
retaybaixo :: Ponto -> Int
retaybaixo (posx,posy) | fromIntegral (ceiling posy) == posy = (ceiling posy) + 1
                       | otherwise = ceiling posy

{-| Função que calcula a distância entre dois pontos
-}
dista :: Maybe Ponto -> Ponto -> Double
dista (Just (x1,y1)) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

{-| Função que dada a posição do carro e a sua velocidade, calcula o ponto da reta que vai intercetar primeiro, a sua distância a este ponto e uma velocidade á qual só é passado o eixo relevante
-}
retaIntersect :: Ponto -> Velocidade -> (Double,Ponto,Velocidade)
retaIntersect (posx,posy) (vx,vy) | intersectX == Nothing && intersectY == Nothing = (0,(posx,posy),(0,0))
                                  | intersectX == Nothing = (dista intersectY (posx,posy),justTo intersectY,(0,vy))
                                  | intersectY == Nothing = (dista intersectX (posx,posy),justTo intersectX,(vx,0))
                                  | dista intersectX (posx,posy) < dista intersectY (posx,posy) = (dista intersectX (posx,posy), justTo intersectX,(vx,0))
                                  | dista intersectX (posx,posy) == dista intersectY (posx,posy) = (dista intersectX (posx,posy), justTo intersectX,(vx,0))
                                  | otherwise = (dista intersectY (posx,posy), justTo intersectY,(0,vy))
    where (rx,ry) = qualRetasIntersect (posx,posy) (vx,vy)
          intersectX = maybeIntersection ((fromIntegral rx,0),(fromIntegral rx,1)) ((posx,posy),(posx + vx,posy + vy))
          intersectY = maybeIntersection ((0,fromIntegral ry),(1,fromIntegral ry)) ((posx,posy),(posx + vx,posy + vy))
{-| Função que retira o tempo que o carro demorou a chegar de uma posição incial de cada ciclo até ao próximo ponto de interceção
-}
retiraTempo :: Tempo -> Ponto -> Velocidade -> Tempo
retiraTempo t (posx,posy) (vx,vy) = t - (d / (sqrt(vx^2 + vy^2)))
    where (d,ponto,v) = retaIntersect (posx,posy) (vx,vy)
{-| Função que calcula a próxima peça que o carro vai intercetar
-}
proximaPeca :: Tabuleiro -> Velocidade -> Ponto -> Peca
proximaPeca t (vx,0) (x,y) | vx < 0 = verificaPecaPos t ((floor x) - 1, floor y)
                           | vx > 0 = verificaPecaPos t ((floor x) + 1, floor y)
                           | otherwise = verificaPecaPos t (floor x, floor y)
proximaPeca t (0,vy) (x,y) | vy < 0 = verificaPecaPos t (floor x, (floor y) -1)
                           | vy > 0 = verificaPecaPos t (floor x, (floor y) +1)
{-| Função que calcula a peça onde o carro se situa no seu primeiro ciclo
-}
pecaAtual :: Tabuleiro -> Ponto -> Peca
pecaAtual t (x,y) = verificaPecaPos t (floor x,floor y)
{-| Função que calcula a peça onde o carro se situa em todos os ciclos menos o primeiro
-}
pecaAtualLimite :: Tabuleiro -> Velocidade -> Ponto -> Peca
pecaAtualLimite t (vx,vy) (x,y) | vx > 0  = verificaPecaPos t ((floor x) - 1,floor y)
                                | vx < 0  = verificaPecaPos t (floor x,floor y)
                                | vy > 0 = verificaPecaPos t (floor x,(floor y) - 1)
                                | vy < 0 = verificaPecaPos t (floor x,floor y)
                                | otherwise = verificaPecaPos t (floor x,floor y)
{-| Função que verifica se o carro entrou em lava

==Exemplo de Utilização
>>> caiNaLava tabCorreto (Peca Recta 0) (2.5,1) (0,(-1)) (Peca Lava 0)
True
-}
caiNaLava :: Tabuleiro -> Peca -> Ponto -> Velocidade -> Peca -> Bool
caiNaLava m (Peca _ a) (posx,posy) (vx,vy) (Peca Lava _) = a >= altLava
caiNaLava m peca1 (posx,posy) (0,0) peca2 = inLava m (posx,posy) peca1
caiNaLava m (Peca (Curva Norte) a) (posx,posy) (vx,vy) peca2 | vx < 0 || vy < 0 = True
                                                             | vx > 0 || vy > 0 = False
caiNaLava m (Peca (Curva Este) a) (posx,posy) (vx,vy) peca2 | vx > 0 || vy < 0 = True
                                                            | vx < 0 || vy > 0 = False
caiNaLava m (Peca (Curva Sul) a) (posx,posy) (vx,vy) peca2 | vx > 0 || vy > 0 = True
                                                           | vx < 0 || vy < 0 = False
caiNaLava m (Peca (Curva Oeste) a) (posx,posy) (vx,vy) peca2 | vx < 0 || vy > 0 = True
                                                             | vx > 0 || vy < 0 = False
caiNaLava m p (posx,posy) (vx,vy) (Peca Recta _) = False
caiNaLava m p (posx,posy) (vx,vy) (Peca (Rampa _) _) = False
caiNaLava m p (posx,posy) (vx,0) (Peca (Curva Norte) _) | vx > 0 = True
                                                        | vx < 0 = False
caiNaLava m p (posx,posy) (vx,0) (Peca (Curva Oeste) _) | vx > 0 = True
                                                      | vx < 0 = False
caiNaLava m p (posx,posy) (vx,0) (Peca (Curva Sul) _) | vx > 0 = False
                                                      | vx < 0 = True
caiNaLava m p (posx,posy) (vx,0) (Peca (Curva Este) _) | vx > 0 = False
                                                       | vx < 0 = True
caiNaLava m p (posx,posy) (0,vy) (Peca (Curva Norte) _) | vy > 0 = True
                                                        | vy < 0 = False
caiNaLava m p (posx,posy) (0,vy) (Peca (Curva Oeste) _) | vy > 0 = False
                                                        | vy < 0 = True
caiNaLava m p (posx,posy) (0,vy) (Peca (Curva Sul) _) | vy > 0 = False
                                                      | vy < 0 = True
caiNaLava m p (posx,posy) (0,vy) (Peca (Curva Este) _) | vy > 0 = True
                                                       | vy < 0 = False
caiNaLava m p1 (posx,posy) (vx,vy) p = True --o ultimo caso desta função dá sempre loop, porque? (por isso pusemos este caso)
    where (d,ponto,(vx,vy)) = retaIntersect (posx,posy) (vx,vy)
{-| Função que dada um velocidade e um ponto, calcula as duas retas que pode possivelmente intercetar
-}
qualRetasIntersect :: Ponto -> Velocidade -> (Int,Int)
qualRetasIntersect (posx,posy) (vx,vy) | vx <= 0 && vy <= 0 = (retaxEsq (posx,posy),retaycima (posx,posy))
                                       | vx <= 0 && vy >= 0 = (retaxEsq (posx,posy),retaybaixo (posx,posy))
                                       | vx >= 0 && vy <= 0 = (retaxDir (posx,posy),retaycima (posx,posy))
                                       | vx >= 0 && vy >= 0 = (retaxDir (posx,posy),retaybaixo (posx,posy))
{-| Função que verifica qual é a peça do tabuleiro de uma determinada posição
-}
verificaPecaPos :: Tabuleiro -> Posicao -> Peca
verificaPecaPos (h:t) (x,0) = verificaPecaPosLinha h x
verificaPecaPos (h:t) (x,y) = verificaPecaPos t (x,y-1)
{-| Função que verifica qual é a peça do tabuleiro de uma determinada posição numa linha
-}
verificaPecaPosLinha :: [Peca] -> Int -> Peca
verificaPecaPosLinha (h:t) 0 = h
verificaPecaPosLinha (h:t) i = (verificaPecaPosLinha t (i-1))
{-| Função que verifica dada uma peça, dá-nos a altura dessa peça
-}
altpeca :: Peca -> Altura
altpeca (Peca _ a) = a
{-| Função que dado um tabuleiro, uma posição e a peça correspondente, verifica se está em lava
-}
inLava :: Tabuleiro -> Ponto -> Peca -> Bool
inLava m (posx,posy) (Peca Lava _) = True
inLava m (posx,posy) (Peca Recta _) = False
inLava m (posx,posy) (Peca (Rampa _) _) = False
inLava m (posx,posy) (Peca (Curva Norte) _) | (posx - (fromIntegral (floor posx))) > (1- (posy - (fromIntegral (floor posy)))) = False
                                            | otherwise = True
inLava m (posx,posy) (Peca (Curva Este) _) | (1 - (posx - (fromIntegral (floor posx)))) > (1- (posy - (fromIntegral (floor posy)))) = False
                                           | otherwise = True
inLava m (posx,posy) (Peca (Curva Sul) _) | (1 - (posx - (fromIntegral (floor posx)))) > (posy - (fromIntegral (floor posy))) = False
                                          | otherwise = True
inLava m (posx,posy) (Peca (Curva Oeste) _) | (posx - (fromIntegral (floor posx))) > (posy - (fromIntegral (floor posy))) = False
                                            | otherwise = True
{-| Função que dadas duas peças, verifica se o carro vai "cair" de nível
-}
caiNivel :: Peca -> Peca -> Bool
caiNivel (Peca _ a1) (Peca (Rampa _) a2) | a2 >= (a1 - 1) = False
                                         | otherwise = True
caiNivel (Peca _ a1) (Peca _ a2) = a2 < a1
{-| Função que dadas duas peças e uma velocidade, verifica se o carro vai colidir com a peça em cima ou em baixo
-}
colideVert :: Peca -> Peca -> Velocidade -> Bool
colideVert (Peca (Rampa _) a1) (Peca _ a2) (vx,vy) | vy == 0 = False
                                                   | a2 <= (a1 + 1) = False
                                                   | otherwise = True
colideVert (Peca _ a1) (Peca _ a2) (vx,vy) | vy == 0 = False
                                           | a2 > a1 = True
                                           | otherwise = False
{-| Função que dadas duas peças e uma velocidade, verifica se o carro vai colidir com a peça à direita ou à esquerda
-}
colideHori :: Peca -> Peca -> Velocidade -> Bool
colideHori (Peca (Rampa _) a1) (Peca _ a2) (vx,vy) | vx == 0 = False
                                                   | a2 <= (a1 + 1) = False
                                                   | otherwise = True
colideHori (Peca _ a1) (Peca _ a2) (vx,vy) | vx == 0 = False
                                           | a2 > a1 = True
                                           | otherwise = False

{-| Tabuleiro de Exemplo
-}
tabCorreto = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Tabuleiro de Exemplo
-}
tabtestes = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Tabuleiro de Exemplo
-}
tabtestes2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca Recta 2,Peca (Rampa Oeste) 1,Peca (Rampa Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Tabuleiro de Exemplo
-}
tabtestes3 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca Recta 2,Peca (Rampa Oeste) 1,Peca (Rampa Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Tabuleiro de Exemplo
-}
tabtestes4 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-| Teste 1
-}
movRetasCurva = (tabtestes2,5,(Carro {posicao = (2.3,1.9), direcao = 0, velocidade = (1,(-0.1))}))
{-| Teste 2
-}
movRetasCurvaDestroi = (tabtestes2,5,(Carro {posicao = (2.3,1.7), direcao = 0, velocidade = (1,(-0.1))}))
{-| Teste 3
-}
movRampasSobe = (tabtestes2,2,(Carro {posicao = (1.7,1.8), direcao = 0, velocidade = (1.2,0.5)}))
{-| Teste 4
-}
movRampasSobeDestroi = (tabtestes2,2.5,(Carro {posicao = (1.7,1.8), direcao = 0, velocidade = (1.2,0.5)}))
{-| Teste 5
-}
movRampasDesce = (tabtestes2,2,(Carro {posicao = (4.7,2.8), direcao = 0, velocidade = (1.2,(-0.5))}))
{-| Teste 6
-}
movRampasDesceCai = (tabtestes2,2,(Carro {posicao = (3.7,2.8), direcao = 0, velocidade = (1.2,(-0.5))}))
{-| Teste 7
-}
movColideRicochete = (tabtestes2,1,(Carro {posicao = (3.7,1.8), direcao = 0, velocidade = (1.2,1)}))
{-| Teste 8
-}
movPassaPorCurva = (tabtestes,0.8,(Carro {posicao = (3.7,1.5), direcao = 0, velocidade = (1.2,1)}))
{-| Teste 9
-}
movAcabaEmCurva = (tabtestes,0.8,(Carro {posicao = (3.7,1.5), direcao = 0, velocidade = (1.2,0.25)}))
{-| Teste 10
-}
movAcabaEmLavaDaCurva = (tabtestes,0.8,(Carro {posicao = (3.7,1.5), direcao = 0, velocidade = (1.2,0.2)}))
{-| Teste 11
-}
movcolide2vezes = (tabtestes4,2,(Carro {posicao = (3.5,3.6), direcao = 90, velocidade = (0.1,(-1))}))
