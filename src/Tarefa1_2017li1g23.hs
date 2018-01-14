{-|
Module : Tarefa1_2017li1g23
Description : Módulo correspondente à 1ª Tarefa do projeto de LI1
Copyright : Nuno Cunha <a85400@alunos.uminho.pt>
            Pedro Parente <a85919alunos.uminho.pt>

Modulo em que foi executada a primeira tarefa do projeto de LI1 com o obectivo de fornecer uma lista de Passos e a partir dela contruir um mapa correspondente
-}
module Tarefa1_2017li1g23 where

import LI11718

{-| Armazena todos os testes a correr para verficar a validade da funcção constroi
-}
testesT1 :: [Caminho]
testesT1 = [cCirculo,cCurvas,cSobeDesce,cCurvasSeguidas,cSobeSobe]

{-| A funcção constroi cria um mapa a partir de um caminho usando o mesmo para obter o ponto de partida, e construir um tabuleiro correspondente

==Exemplo de Utilização
>>> constroi [Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir]
Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-}
constroi :: Caminho -> Mapa
constroi c = Mapa (partida c, dirInit) (caminhoTabuleiro tabInit c altInit dirInit (partida c))
    where tabInit = tabuleiroLava (dimensao c)

{-| Função que constrói uma linha de peças do tipo Lava
-}
tabuleiroLavaLinha :: Int-> [Peca]
tabuleiroLavaLinha n = replicate n (Peca Lava altLava)

{-| Função que constrói o tabuleiro apenas com peças do tipo Lava

==Exemplo de Utilização
>>> tabuleiroLava (2,2)
[[Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0]]
-}
tabuleiroLava :: Dimensao -> Tabuleiro
tabuleiroLava (c,l) = replicate l (tabuleiroLavaLinha c) 

{-| Função que atualiza uma peça de uma linha do Tabuleiro 
-}
actualizaTabuleiroLinha :: [Peca] -> Peca -> Int -> [Peca]
actualizaTabuleiroLinha (h:t) p 0 = p:t
actualizaTabuleiroLinha (h:t) p i = h:(actualizaTabuleiroLinha t p (i-1))

{-| Função que atualiza uma peça do Tabuleiro

==Exemplo de Utilização 
>>> actualizaTabuleiro [[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0]] (Peca Recta 1) (1,1)
[[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-}
actualizaTabuleiro :: Tabuleiro -> Peca -> Posicao -> Tabuleiro
actualizaTabuleiro (c:t) peca (x,0) = actualizaTabuleiroLinha c peca x : t
actualizaTabuleiro (c:t) peca (x,y) = c:actualizaTabuleiro t peca (x,y-1)

{-| Função principal que partindo de um caminho e fornecendo ainda um tabuleiro, uma altura, orientação e uma posição inicial, ela utiliza todas as outras funções auxiliares para construir o tabuleiro final

==Exemplo de Utilização
>>> caminhoTabuleiro [Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir]
[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-}
caminhoTabuleiro :: Tabuleiro -> Caminho -> Altura -> Orientacao -> Posicao -> Tabuleiro
caminhoTabuleiro tab [] a o (x,y) = tab
caminhoTabuleiro tab (p:t) a o (x,y) = caminhoTabuleiro aTab t nAlt nOri nPos 
    where aTab = actualizaTabuleiro tab (passoPeca p o a) (x,y)
          nAlt = novaAltura p a
          nOri = novaOrientacao p o
          nPos = novaPosicao (x,y) p o

{-| Função que vai atualizando qual a altura que vai ser usada pela função principal, à medida que o caminho é construído
-}
novaAltura :: Passo -> Altura -> Altura
novaAltura Sobe a = a + 1
novaAltura Desce a = a - 1
novaAltura CurvaDir a = a
novaAltura CurvaEsq a = a
novaAltura Avanca a = a

{-| Função que vai atualizando qual a posição do tabuleiro que vai ser usada pela função principal, à medida que o caminho é construído
-}
novaPosicao :: Posicao -> Passo -> Orientacao -> Posicao
novaPosicao (x,y) p o | (novaOrientacao p o) == Norte = (x,y - 1)
                      | (novaOrientacao p o) == Este = (x + 1,y)
                      | (novaOrientacao p o) == Sul = (x, y + 1)
                      | (novaOrientacao p o) == Oeste = (x - 1,y)

{-| Função que dá novas orientações a cada passo de um caminho tendo apenas efeito visível em curvas
-}
novaOrientacao :: Passo -> Orientacao -> Orientacao
novaOrientacao Avanca o = o
novaOrientacao Sobe o = o
novaOrientacao Desce o = o
novaOrientacao CurvaDir o | o == Norte = Este
                          | o == Este = Sul
                          | o == Sul = Oeste
                          | o == Oeste = Norte
novaOrientacao CurvaEsq o | o == Norte = Oeste
                          | o == Este = Norte
                          | o == Sul = Este
                          | o == Oeste = Sul

{-| Função que transforma um passo na sua Peça correspondente
-}
passoPeca :: Passo -> Orientacao -> Altura-> Peca
passoPeca Avanca o a = (Peca Recta) a
passoPeca Sobe o a = (Peca (Rampa o)) a
passoPeca Desce o a = (Peca (Rampa (orientaRampaDesce o))) (a-1)
passoPeca CurvaDir o a = (Peca (Curva o)) a
passoPeca CurvaEsq o a = (Peca (Curva (orientaCurvaEsq o))) a

{-| Função que dá uma nova orientação à peça Rampa no caso de estar descer
-}
orientaRampaDesce :: Orientacao -> Orientacao
orientaRampaDesce Norte = Sul
orientaRampaDesce Este = Oeste
orientaRampaDesce Sul = Norte
orientaRampaDesce Oeste = Este

{-| Função que dá uma nova orientação à peça Curva no caso de o passo correspondente ser "CurvaEsq"
-}
orientaCurvaEsq ::  Orientacao -> Orientacao
orientaCurvaEsq Norte = Este
orientaCurvaEsq Este = Sul
orientaCurvaEsq Sul = Oeste
orientaCurvaEsq Oeste = Norte

{-| Caminho Teste 1
-}
cCirculo = [Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir]
{-| Caminho Teste 2
-}
cCurvas = [Avanca, CurvaEsq, Avanca, CurvaEsq, Sobe, Avanca, Desce, Avanca, CurvaEsq, Sobe, Avanca, Avanca, CurvaEsq, CurvaDir, Avanca, CurvaEsq, Avanca, CurvaEsq, Avanca, Desce, CurvaEsq, Avanca, CurvaDir, CurvaDir, Avanca, Avanca]
{-| Caminho Teste 3
-}
cSobeDesce = [Avanca, Sobe, Desce, CurvaDir, CurvaDir, Sobe, Desce, Avanca, CurvaDir, CurvaDir]
{-| Caminho Teste 4
-}
cCurvasSeguidas = [Avanca,CurvaDir,CurvaDir,CurvaEsq,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,CurvaDir,Avanca,Avanca,Avanca,CurvaDir]
{-| Caminho Teste 5
-}
cSobeSobe = [Sobe,Sobe,Avanca,Desce,Desce,CurvaDir,CurvaDir,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaDir,CurvaDir]