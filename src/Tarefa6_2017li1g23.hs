{-|
Module      : Tarefa6_2017li1g23
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g23 where

import LI11718
import Tarefa2_2017li1g23

{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot t (Jogo {mapa = daMap, pista = Propriedades {k_atrito = 1, k_pneus = 1, k_acel = 2, k_peso = 0.5, k_nitro = 4, k_roda = 180},carros = [Carro {posicao = (5.5,3.5), direcao = 0, velocidade = (0,0)}],nitros = [5],historico = [[]]}) 0 = Acao {acelerar = True,travar = False,esquerda = False,direita = False,nitro = Just 0}

