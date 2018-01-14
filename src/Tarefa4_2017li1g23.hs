{-|
Module      : Tarefa4_2017li1g23
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g23 where

import LI11718

{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [exe1,exevy,exevx,exevxy,exerampa,exenitro,exe2carro,exenitro2]

{-|
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.
-}
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
atualiza t (Jogo {mapa = m, pista = Propriedades {k_atrito = katrito, k_pneus = kpneus, k_acel = kacel, k_peso = kpeso, k_nitro = knitro, k_roda = kroda},carros = lc,nitros = ln,historico = lh}) jog (Acao {acelerar = acel, travar = trav, esquerda = esq, direita = dir, nitro = isNitro}) = Jogo {mapa = m, pista = Propriedades {k_atrito = katrito, k_pneus = kpneus, k_acel = kacel, k_peso = kpeso, k_nitro = knitro, k_roda = kroda},carros = (carroNitroCerto (tempoNitro isNitro t (ln !! jog)) f_Nitro (carroCerto t (Jogo {mapa = m, pista = Propriedades {k_atrito = katrito, k_pneus = kpneus, k_acel = kacel, k_peso = kpeso, k_nitro = knitro, k_roda = kroda},carros = lc,nitros = ln,historico = lh}) (Acao {acelerar = acel, travar = trav, esquerda = esq, direita = dir, nitro = isNitro}) jog jog lc ln) isNitro),nitros = nitroCerto ln jog isNitro t,historico = actualizaHist lh (listaPosCarros lc)}
  where f_Nitro = fNitro isNitro knitro (angCarroCerto lc isNitro)
{-| Função que dado o tempo da Ação, o Jogo, a Ação, o identificador do jogador, a lista de carros e a lista de tempos de nitro, calcula, através de um contador, qual é o carro que vai ser o "alvo" da Ação.
-}
carroCerto :: Tempo -> Jogo -> Acao -> Int -> Int -> [Carro] -> [Tempo] -> [Carro]
carroCerto tAcao jogo a jog 0 lc ln = carroMuda tAcao jog jogo a lc
carroCerto tAcao jogo a jog n (h:t) ln = h:(carroCerto tAcao jogo a jog (n-1) t ln)
{-| Função que dado o tempo da Ação, o identificador do jogador, o Jogo, a Ação e a lista de carros, devolve o a lista de carros com o carro alvo já sob o efeito da Ação.
-}
carroMuda :: Tempo -> Int -> Jogo -> Acao -> [Carro] -> [Carro]
carroMuda tAcao jog (Jogo {mapa = Mapa _ tab, pista = Propriedades {k_atrito = katrito, k_pneus = kpneus, k_acel = kacel, k_peso = kpeso, k_nitro = knitro, k_roda = kroda},carros = lc,nitros = ln,historico = lh}) (Acao {acelerar = acel,travar = trav,esquerda = esq,direita = dir,nitro = isNitro}) (Carro {posicao = (posx,posy), direcao = ang, velocidade = (vx,vy)}:t) = (Carro {posicao = (posx,posy),direcao = actualizaDir ang esq dir kroda tAcao, velocidade = (newVeloc tAcao (vx,vy) (somaForcas f_pneus f_atrito f_acel f_gravity))}:t)
  where f_pneus = paraCartesinas (fpneus (vx,vy) ang kpneus)
        f_atrito = fatrito (vx,vy) katrito
        f_acel = facel kacel acel trav ang
        f_gravity = fgravity kpeso (verificaPecaPos tab (floor posx,floor posy))
        f_Nitro = fNitro isNitro knitro (angCarroCerto lc isNitro)
{-| Função que dado o Ângulo, um Bool sobre se o carro vira à esquerda, um Bool sobre se o carro vira à direita, o k_roda e o tempo da Ação, calcula a direção final do carro.
-}
actualizaDir :: Angulo -> Bool -> Bool -> Double -> Tempo -> Angulo
actualizaDir ang True False kroda t = ang + t*kroda
actualizaDir ang False True kroda t = ang - t*kroda
actualizaDir ang _ _ _ _ = ang
{-| Função que dada uma lsita de carros devolve uma lista de posições desses carros
-}
listaPosCarros :: [Carro] -> [Posicao]
listaPosCarros [] = []
listaPosCarros ((Carro {posicao = (x,y), direcao = _, velocidade = _}):cs) = (floor x,floor y):(listaPosCarros cs)
{-| Função que dado o histórico dos carros, o identificador do jogador e a posição do jogador em causa, calcula o histórico atualizado, tendo em conta se o jogador já esteve nesse sítio ou não.
-}
actualizaHist :: [[Posicao]] -> [Posicao] -> [[Posicao]]
actualizaHist (h:t) [(x,y)] | elem (x,y) h = h:t
                            | otherwise = ((x,y):h):t
actualizaHist (h:t) ((x,y):ps) | elem (x,y) h = h:(actualizaHist t ps)
                               | otherwise = ((x,y):h):(actualizaHist t ps)
{-| Função que dado o tempo de Ação, a velocidade antes de ser aplicada a Ação, e as forças consequentes da ação (que vão ser calculadas por outras funções), calcula a velocidade final (antes de se aplicar o nitro).
-}
newVeloc :: Tempo -> Velocidade -> (Double,Double) -> Velocidade
newVeloc t (vx,vy) (fx,fy) = (vx + (t*fx), vy + (t*fy))
{-| Função que dado a lista de carros e o estado do nitro, calcula qual o Ângulo do carro alvo do nitro (que pode ser ou não o jogador que o ativa).
-}
angCarroCerto :: [Carro] -> Maybe Int -> Angulo
angCarroCerto (Carro {posicao = (posx,posy), direcao = ang, velocidade = (vx,vy)}:t) (Just 0) = ang
angCarroCerto (h:t) (Just a) = angCarroCerto t (Just (a - 1))
{-| Função que dado a lista de carros e o identificador do jogador, calcula qual a posição do tabuleiro onde se encontra.
-}
posCarroCerto :: [Carro] -> Int -> Posicao
posCarroCerto (Carro {posicao = (posx,posy), direcao = ang, velocidade = (vx,vy)}:t) 0 = (floor posx,floor posy)
posCarroCerto (h:t)  a = posCarroCerto t (a - 1)
{-| Função que dado o tempo de nitro, um par com a força do nitro no eixo do x e do y, a lista de carros e o estado do nitro, calcula a lista de carros com o carro alvo já sob o efeito do nitro.
-}
carroNitroCerto :: Tempo -> (Double,Double) -> [Carro] -> Maybe Int -> [Carro]
carroNitroCerto _ _ lc Nothing = lc
carroNitroCerto tn (nx,ny) (Carro {posicao = pos, direcao = ang, velocidade = (vx,vy)}:t) (Just 0) = (Carro {posicao = pos, direcao = ang, velocidade = newVelocNitro (vx,vy) (nx,ny) tn}:t)
carroNitroCerto tn (nx,ny) (h:t) (Just a) = h:carroNitroCerto tn (nx,ny) t (Just (a - 1))
{-| Função auxiliar da carroNitroCerto que faz a parte do cálculo da velocidade final.
-}
newVelocNitro :: Velocidade -> (Double,Double) -> Tempo -> Velocidade
newVelocNitro (vx,vy) (nx,ny) tn = (vx + nx*tn,vy + ny*tn)
{-| Função que soma as forças todas, exceto do nitro.
-}
somaForcas :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double)
somaForcas fpneus fatrito facel fgravity = (fx,fy)
    where fx = fst(fpneus)+fst(fatrito)+fst(facel)+fst(fgravity)
          fy = snd(fpneus)+snd(fatrito)+snd(facel)+snd(fgravity)
{-| Função que calcula a força consequente do k_pneus.
-}
fpneus :: Velocidade -> Angulo -> Double -> (Double,Double)
fpneus (vx,vy) ang kpneus | vx == 0 && vy == 0 = (0,0)
                          | (mod (fromEnum ang) 360) < 180 = if passaZero then (abs (sin(difAngs) * kpneus * sqrt(vx^2+vy^2)), ang + 90) else (abs (sin(difAngs) * kpneus * sqrt(vx^2+vy^2)), ang - 90)
                          | otherwise = if passaZero then (sin(difAngs) * kpneus * sqrt(vx^2+vy^2), ang - 90) else (abs (sin(difAngs) * kpneus * sqrt(vx^2+vy^2)), ang + 90)
    where difAngs = angVel (vx,vy) - (((fromIntegral(mod (fromEnum ang) 360))*pi)/180)
          passaZero = (((angVel (vx,vy))*180)/pi) < (fromIntegral(mod (fromEnum ang) 360)) || (((angVel (vx,vy))*180)/pi) > (fromIntegral(mod ((fromEnum ang)-180) 360))
{-| Função auxiliar da fpneus
-}
angVel :: Velocidade -> Angulo
angVel (vx,vy) | vx > 0 = atan((-vy)/vx)
               | vx < 0 = (atan((-vy)/vx) + pi)
               | vy < 0 = pi/2
               | vy > 0 = (3*pi)/2
{-| Função que calcula a força de atrito.
-}
fatrito :: Velocidade -> Double -> (Double,Double)
fatrito (vx,vy) katrito = (-(katrito * vx),-(katrito * vy))
{-| Função que calcula a força de aceleração.
-}
facel :: Double -> Bool -> Bool -> Angulo -> (Double,Double)
facel kacel acelera trava ang | acelera = (kacel * cos((ang * pi)/180),-(kacel * sin((ang * pi)/180)))
                              | trava = (-(kacel * cos((ang * pi)/180)),kacel * sin((ang * pi)/180))
                              | otherwise = (0,0)
{-| Função que calcula a força da gravidade sobre o carro (apenas relevante quando o carro se situa sobre uma rampa).
-}
fgravity :: Double -> Peca -> (Double,Double)
fgravity kpeso (Peca (Rampa Sul) _) = (0,-(kpeso))
fgravity kpeso (Peca (Rampa Norte) _) = (0,kpeso)
fgravity kpeso (Peca (Rampa Este) _) = (-(kpeso),0)
fgravity kpeso (Peca (Rampa Oeste) _ ) = (kpeso,0)
fgravity kpeso _ = (0,0)
{-| Função que dado a lista com os tempos do nitro, um contador, o estado do nitro e o tempo do Ação, calcula qual o  carro que se deve atualizar o tempo de nitro e faz a atualização.
-}
nitroCerto :: [Tempo] -> Int -> Maybe Int -> Tempo -> [Tempo]
nitroCerto lnitro n Nothing tacao = lnitro
nitroCerto (tnitro:t) 0 isnitro tacao = (actualizaNitroTempo tacao tnitro):t
nitroCerto (h:t) n isnitro tacao = h:(nitroCerto t (n-1) isnitro tacao)
{-| Função que dado o estado do nitro, o tempo da Ação, e o tempo do nitro restante para um determinado carro, calcula o tempo que vai usar no cálculo da velocidade resultante da ação do nitro.
-}
tempoNitro :: Maybe Int -> Tempo -> Tempo -> Tempo
tempoNitro nit tacao tnitro | nit == Nothing = tacao
                            | tnitro > tacao = tacao
                            | otherwise = tnitro
{-| Função que dado o estado do nitro, o k_nitro e o Ângulo, calcula a força do nitro.
-}
fNitro :: Maybe Int -> Double -> Angulo -> (Double,Double)
fNitro Nothing _ _ = (0,0)
fNitro _ knitro ang = (knitro * cos((ang * pi)/180),-(knitro * sin((ang * pi)/180)))
{-| Função auxiliar da nitroCerto, trata da atualização dos tempo de nitro.
-}
actualizaNitroTempo :: Tempo -> Tempo -> Tempo
actualizaNitroTempo tacao tnitro | tnitro - tacao <= 0 = 0
                                 | otherwise = tnitro - tacao
{-| Função que passa coordenadas polares para cartesianas.
-}
paraCartesinas :: (Double,Double) -> (Double,Double)
paraCartesinas (f,ang) = (cos((ang*pi)/180)*f,-sin((ang*pi)/180)*f)
{-| Função que verifica qual é a peça do tabuleiro de uma determinada posição.
-}
verificaPecaPos :: Tabuleiro -> Posicao -> Peca
verificaPecaPos (h:t) (x,0) = verificaPecaPosLinha h x
verificaPecaPos (h:t) (x,y) = verificaPecaPos t (x,y-1)
{-| Função que verifica qual é a peça do tabuleiro de uma determinada posição numa linha
-}
verificaPecaPosLinha :: [Peca] -> Int -> Peca
verificaPecaPosLinha (h:t) 0 = h
verificaPecaPosLinha (h:t) i = (verificaPecaPosLinha t (i-1))
{-| Teste 1
-}
exe1 = (1,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (2.5,1.5), direcao = 180, velocidade = (0,0)}],nitros = [5],historico = [[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Nothing})
{-| Teste 2
-}
exenitro = (0.2,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (2.5,1.5), direcao = 180, velocidade = (0,0)}],nitros = [5],historico = [[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Just 0})
{-| Teste 3
-}
exe2carro = (1,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (2.5,1.5), direcao = 180, velocidade = (0,0)},Carro {posicao = (2.5,3.5), direcao = 0, velocidade = (0.5,0)}],nitros = [5,5],historico = [[],[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Nothing})
{-| Teste 4
-}
exenitro2 =(1,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (2.5,1.5), direcao = 180, velocidade = (0,0)},Carro {posicao = (2.5,3.5), direcao = 0, velocidade = (0,0)}],nitros = [5,5],historico = [[],[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Just 1})
{-| Teste 5
-}
exevy = (1,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (2.5,1.5), direcao = -200, velocidade = (0,1)}],nitros = [5],historico = [[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Nothing})
{-| Teste 6
-}
exevx = (1,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (2.5,1.5), direcao = 140, velocidade = (1,0)}],nitros = [5],historico = [[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Nothing})
{-| Teste 7
-}
exevxy = (1,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (2.5,1.5), direcao = 80, velocidade = (1,1)}],nitros = [5],historico = [[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Nothing})
{-| Teste 8
-}
exerampa = (0.6,Jogo {mapa = (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),pista = Propriedades {k_atrito = 0.2, k_pneus = 2,k_acel = 4,k_peso = 2,k_nitro = 15, k_roda = 180},carros = [Carro {posicao = (1.6,1.55), direcao = -90, velocidade = (0,1)}],nitros = [5],historico = [[]] }, Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Nothing})
