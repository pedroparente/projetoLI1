{-|
Module : Tarefa2_2017li1g23
Description : Módulo correspondente à 2ª Tarefa do projeto de LI1
Copyright : Nuno Cunha <a85400@alunos.uminho.pt>
            Pedro Parente <a85919alunos.uminho.pt>

Módulo em que foi executada a segunda tarefa do projeto de LI1 com o objetivo de verificar se um determinadado mapa segue uns certos parâmetros, permitindo a sua jogabilidade
-}
module Tarefa2_2017li1g23 where

import LI11718

{-| Armazena todos os testes a correr para verificar a validade da Função valida
-}
testesT2 :: [Tabuleiro]
testesT2 = [tabCorreto,tabBordaMal,tabBordaMalLados,tabPecaFora,tabPercMal1,tabPercMal2,tabPercMal3,tabAltura,tabAlturaSobeDesce,tabAlturaMal1,tabAlturaMal2,tabAlturaSobeDesceMal,tabOriInitMal,tabAltLavaMal]

{-| A Função valida, como o próprio nome indica, usa um mapa e todas as funções auxiliares para validar esse mesmo mapa, de acordo com as regras fornecidas 

==Exemplos de Utilização
>>> valida (Mapa ((2,1),Este) tabCorreto)
True
>>> valida tabBordaMal
False
-}
valida :: Mapa -> Bool
valida m = altLavaValida m && bordaLava m && validaOrientacaoInit m && validaTraj m && validaAltura m && verExistePercurso m 

{-| Função que valida se a altura de todas as peças lava é a correta

==Exemplo de Utilização
>>> altLavaValida (Mapa ((2,1),Este) tabAltLavaMal)
False
-}
altLavaValida :: Mapa -> Bool
altLavaValida (Mapa _ []) = True
altLavaValida (Mapa i (h:t)) | altLavaValidaLinha h = altLavaValida (Mapa i t)
                       | otherwise = False
{-| Função que valida se uma linha do tabuleiro tem a altura de lava correta
-}
altLavaValidaLinha :: [Peca] -> Bool
altLavaValidaLinha [] = True
altLavaValidaLinha ((Peca Lava a):t) | a == altLava = altLavaValidaLinha t
                                     | otherwise = False
altLavaValidaLinha (_:t) = altLavaValidaLinha t

{-| Função que verifica se as bordas do tabuleiro são lava
-}
bordaLava :: Mapa -> Bool
bordaLava (Mapa _ (h:t)) = topoLava h && fundoLava (last t) && ladosLava t

{-| Função que verifica se o topo do tabuleiro é lava
-}
topoLava :: [Peca] -> Bool
topoLava [] = True
topoLava (h:t) | h == (Peca Lava 0) = topoLava t
               | otherwise = False

{-| Função que verifica se o fundo do tabuleiro é lava
-}
fundoLava :: [Peca] -> Bool
fundoLava [] = True
fundoLava (h:t) | h == (Peca Lava 0) = fundoLava t
                | otherwise = False

{-| Função que verifica se os lados do tabuleiro são lava
-}
ladosLava :: Tabuleiro -> Bool
ladosLava [h] = True
ladosLava (h:t) | ladosLavaLinha h = ladosLava t
                | otherwise = False 

{-| Função que verifica se os extremos de cada linha do tabuleiro são lava
-}
ladosLavaLinha :: [Peca] -> Bool
ladosLavaLinha (h:t) = esquerdaLava h && direitaLava (last t)
{-| Função que verifica se o extremo esquerdo de uma linha do tabuleiro é lava
-}
esquerdaLava :: Peca -> Bool
esquerdaLava (Peca Lava 0) = True
esquerdaLava _ = False
{-| Função que verifica se o extremo direito de uma linha do tabuleiro é lava
-}
direitaLava :: Peca -> Bool
direitaLava (Peca Lava 0) = True
direitaLava _ = False

--IMPORTANTE Funcões Auxiliares
{-| Função que dado uma Orientação atual decide qual a proxima orientação a seguir com base na peça em que nos encontramos 
-}
pecaMudaOrien :: Orientacao -> Peca -> Orientacao
pecaMudaOrien o (Peca Recta _)= o
pecaMudaOrien o (Peca (Rampa _) _) = o
pecaMudaOrien Norte (Peca (Curva r) _) | r == Norte = Este
                                       | r == Este = Oeste
pecaMudaOrien Este (Peca (Curva r) _) | r == Este = Sul
                                      | r == Sul = Norte
pecaMudaOrien Sul (Peca (Curva r) _) | r == Sul = Oeste
                                     | r == Oeste = Este
pecaMudaOrien Oeste (Peca (Curva r) _) | r == Oeste = Norte
                                       | r == Norte = Sul

{-| Funcão que decide a proxima posição a verificar com base na posição e na orientação atual
-}
posicaoNext :: Posicao -> Orientacao-> Posicao
posicaoNext (x,y) Norte = (x, y - 1)
posicaoNext (x,y) Este = (x + 1,y)
posicaoNext (x,y) Sul = (x,y + 1)
posicaoNext (x,y) Oeste = (x - 1,y)

{-| Função que verifica a ultima peça do tabuleiro para servir de caso de paragem á verificação
-}
ultimaPeca :: Posicao -> Orientacao-> Posicao
ultimaPeca (x,y) Norte = (x, y + 1)
ultimaPeca (x,y) Este = (x - 1,y)
ultimaPeca (x,y) Sul = (x,y - 1)
ultimaPeca (x,y) Oeste = (x + 1,y)
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


{-| Função que verifica se o percurso corresponde a uma trajetória circlar que começa e termina no mesmo ponto, com a mesma orientação

==Exemplo de Utilização
>>> validaTraj (Mapa ((2,1),Este) tabPercMal1)
False
-}
validaTraj :: Mapa -> Bool
validaTraj (Mapa (p,o) t) = percCircular p nextOrienta (posicaoNext p nextOrienta) (verificaPecaPos t (posicaoNext p nextOrienta)) (ultimaPeca p o) o t
    where nextOrienta = pecaMudaOrien o (verificaPecaPos t p)
{-| Funcão que usa todos os dados fornecidos na validaTraj para verficar se a trajetória é válida
-}
percCircular :: Posicao -> Orientacao -> Posicao -> Peca -> Posicao -> Orientacao -> Tabuleiro -> Bool
percCircular p o p2 (Peca Lava 0) pUlt oInit t = False
percCircular p Norte p2 (Peca (Curva Oeste) _) pUlt oInit t = False
percCircular p Norte p2 (Peca (Curva Sul) _) pUlt oInit t = False
percCircular p Norte p2 (Peca (Rampa Este) _) pUlt oInit t = False
percCircular p Norte p2 (Peca (Rampa Oeste) _) pUlt oInit t = False
percCircular p Sul p2 (Peca (Curva Este) _) pUlt oInit t = False
percCircular p Sul p2 (Peca (Curva Norte) _) pUlt oInit t = False
percCircular p Sul p2 (Peca (Rampa Oeste) _) pUlt oInit t = False
percCircular p Sul p2 (Peca (Rampa Este) _) pUlt oInit t = False
percCircular p Este p2 (Peca (Curva Oeste) _) pUlt oInit t = False
percCircular p Este p2 (Peca (Curva Norte) _) pUlt oInit t = False
percCircular p Este p2 (Peca (Rampa Norte) _) pUlt oInit t = False
percCircular p Este p2 (Peca (Rampa Sul) _) pUlt oInit t = False
percCircular p Oeste p2 (Peca (Curva Este) _) pUlt oInit t = False
percCircular p Oeste p2 (Peca (Curva Sul) _) pUlt oInit t = False
percCircular p Oeste p2 (Peca (Rampa Norte) _) pUlt oInit t = False
percCircular p Oeste p2 (Peca (Rampa Sul) _) pUlt oInit t = False
percCircular p o p2 peca2 pUlt oInit t | p == pUlt && o == oInit = True
                                       | otherwise = percCircular p2 nextOrienta (posicaoNext p2 nextOrienta) (verificaPecaPos t (posicaoNext p2 nextOrienta)) pUlt oInit t
    where nextOrienta = pecaMudaOrien o (verificaPecaPos t p2)

--Funcões que dão listas do caminho ordenado
{-| Funcção que dá todas as pecas do tabuleiro sem ser lava por ordem (é apenas usada depois de validaTraj)
-}
pecasCaminho :: [Posicao] -> Tabuleiro -> [Peca]
pecasCaminho [] _ = []
pecasCaminho (p:t) tab = (verificaPecaPos tab p):(pecasCaminho t tab)

{-| Funcção que dá todas as posições do tabuleiro que não contêm uma Peca Lava por ordem (é apenas usada depois de validaTraj)
-}
posicoesCaminho :: Posicao -> Orientacao -> Tabuleiro -> Posicao -> [Posicao]
posicoesCaminho p o t pUlt | p == pUlt = [p]
                           | otherwise = p:(posicoesCaminho (posicaoNext p nextOrienta) nextOrienta t pUlt)
    where nextOrienta = pecaMudaOrien o (verificaPecaPos t p)


{-|Função que valida a Orientação inicial

==Exemplo de Utilização
>>> validaOrientacaoInit (Mapa ((2,1),Este) tabOriInitMal)
False
-}
validaOrientacaoInit :: Mapa -> Bool
validaOrientacaoInit (Mapa (p,o) t) = comparaOrientacao peca o
    where peca = (verificaPecaPos t p)

{-| Funcção que com dados fornecidos na função validaOrientacaoInit verifica se orientação inicial é compativel com a 1ª Peça 
-}
comparaOrientacao :: Peca -> Orientacao -> Bool
comparaOrientacao (Peca Recta _) o = True
comparaOrientacao (Peca (Curva Sul) _) o = comparaCNorteSul o
comparaOrientacao (Peca (Curva Norte) _) o = if comparaCNorteSul o then False else True
comparaOrientacao (Peca (Curva Este) _) o = comparaCEsteOeste o
comparaOrientacao (Peca (Curva Oeste) _) o = if comparaCEsteOeste o then False else True
comparaOrientacao (Peca (Rampa Sul) _) o = comparaRNorteSul o
comparaOrientacao (Peca (Rampa Norte) _) o = comparaRNorteSul o
comparaOrientacao (Peca (Rampa Este) _) o = comparaREsteOeste o
comparaOrientacao (Peca (Rampa Oeste) _) o = comparaREsteOeste o
comparaOrientacao p m = False

{-| Função que verifica se ao começar numa Curva Norte ou Sul a orientação é válida
-}
comparaCNorteSul :: Orientacao -> Bool
comparaCNorteSul o | o == Este = True
                   | o == Oeste = False
                   | o == Norte = False
                   | o == Sul = True
{-| Função que verifica se ao começar numa Curva Este ou Oeste a orientação é válida
-}
comparaCEsteOeste :: Orientacao -> Bool
comparaCEsteOeste o | o == Este = True
                    | o == Oeste = False
                    | o == Norte = True
                    | o == Sul = False
{-| Função que verifica se ao começar numa Rampa Norte ou Sul a orientação é válida
-}
comparaRNorteSul :: Orientacao -> Bool
comparaRNorteSul o | o == Este = False
                   | o == Oeste = False
                   | o == Norte = True
                   | o == Sul = True
{-| Função que verifica se ao começar numa Rampa Este ou Oeste a orientação é válida
-}
comparaREsteOeste :: Orientacao -> Bool
comparaREsteOeste o | o == Este = True
                    | o == Oeste = True
                    | o == Norte = False
                    | o == Sul = False

{-| Funcção que valida Alturas com base na lista de peças ordenadas

==Exemplos de Utilização
>>> validaAltura (Mapa ((2,1),Este) tabAlturaMal1)
False
-}
validaAltura :: Mapa -> Bool
validaAltura (Mapa (p,o) t) = validaAlturaAux (pecasCaminho (posicoesCaminho p o t (ultimaPeca p o)) t) o
  where nextOrienta = pecaMudaOrien o (verificaPecaPos t p)

{-| Função que usando todos os dados fornecidos na validaAltura testa recursivamente sobre a lista de peças ordenadas se a altura é compativel com a peça seguinte
-}
validaAlturaAux :: [Peca] -> Orientacao -> Bool
validaAlturaAux [p] _ = True
validaAlturaAux ((Peca (Rampa o1) a1):(Peca (Rampa o2) a2):t) ori | o1 == o2 && o1 == ori = (a1 + 1) == a2 && validaAlturaAux ((Peca (Rampa o2) a2):t) (pecaMudaOrien ori (Peca (Rampa o1) a1))
                                                                  | o1 == o2 && o1 /= ori = (a1 - 1) == a2 && validaAlturaAux ((Peca (Rampa o2) a2):t) (pecaMudaOrien ori (Peca (Rampa o1) a1))
                                                                  | otherwise = a1 == a2  && validaAlturaAux ((Peca (Rampa o2) a2):t) (pecaMudaOrien ori (Peca (Rampa o1) a1))
validaAlturaAux ((Peca (Rampa o) a):p2:t) ori | o == ori = ((a + 1) == altPeca p2) && validaAlturaAux (p2:t) (pecaMudaOrien ori (Peca (Rampa o) a))
                                              | otherwise = a == altPeca p2 && validaAlturaAux (p2:t) (pecaMudaOrien ori (Peca (Rampa o) a))
validaAlturaAux (p1:(Peca (Rampa o) a):t) ori | o == (pecaMudaOrien ori p1) = a == altPeca p1 && validaAlturaAux ((Peca (Rampa o) a):t) (pecaMudaOrien ori p1)
                                              | otherwise = (a + 1) == altPeca p1 && validaAlturaAux ((Peca (Rampa o) a):t) (pecaMudaOrien ori p1)
validaAlturaAux (p1:p2:t) ori | (altPeca p1) == (altPeca p2) = validaAlturaAux (p2:t) (pecaMudaOrien ori p1)
                              | otherwise = False  
{-| Função que verifica dada uma peça, dá-nos a altura dessa peça
-}
altPeca :: Peca -> Altura
altPeca (Peca _ a) = a

{-| Função que verifica se existe apenas um Percurso, vendo se existem mais peças sem ser do tipo lava no tabuleiro do que as formadas pelo caminho

==Exemplo de Utilização
>>> verExistePercurso (Mapa ((2,1),Este) tabPecaFora)
False
-}
verExistePercurso :: Mapa -> Bool
verExistePercurso (Mapa (p,o) t) = nPecasCam == nPecasTab
    where nextOrienta = pecaMudaOrien o (verificaPecaPos t p)
          nPecasCam = length (posicoesCaminho p o t (ultimaPeca p o))
          nPecasTab = length (nPecasNoTabuleiro t)
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


{-| Teste 1
-}
tabCorreto = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 2
-}
tabBordaMal = [[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 3
-}
tabBordaMalLados = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca (Curva Este) 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 4
-}
tabPecaFora = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Recta 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 5
-}
tabPercMal1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Este) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 6
-}
tabPercMal2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 7
-}
tabPercMal3 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Oeste) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 8
-}
tabAltura = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca (Rampa Este) 2,Peca Recta 3,Peca (Curva Este) 3,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca (Rampa Este) 2,Peca Recta 3,Peca (Curva Sul) 3,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 9
-}
tabAlturaSobeDesce = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 10
-}
tabAlturaSobeDesceMal = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 1,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 11
-}
tabAlturaMal1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 1,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 12
-}
tabAlturaMal2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 2,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 13
-}
tabOriInitMal = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
{-| Teste 14
-}
tabAltLavaMal = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 1,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]