import Data.List (nub)
import System.IO

type ReservedWordCount = (String, Int) -- Palavra reservada e contagem de quantas vezes ela aparece

-- Caminho do arquivo de entrada para as palavras reservadas
nameFileRes :: FilePath
nameFileRes = "res.txt"

-- Caminho do arquivo de entrada para os separadores
nameFileSep :: FilePath
nameFileSep = "sep.txt"

-- Caminho do arquivo de entrada para o primeiro programa
nameFileC1 :: FilePath
nameFileC1 = "c1.txt"

-- Caminho do arquivo de entrada para o segundo programa
nameFileC2 :: FilePath
nameFileC2 = "c2.txt"

-- Aplicação da regra do peso
applyWeights :: [(String, Int)] -> [String] -> [(String, Int)]
applyWeights [] _ = [] -- Se a lista de palavras reservadas for nula, retorna uma lista nula
applyWeights ((word, count) : rest) reservedWords
  | isReservedWord word reservedWords = (word, count * 2) : applyWeights rest reservedWords -- Se a palavra for uma palavra reservada, multiplica a contagem por 2 e concatena com outras iterações da função
  | otherwise = (word, count) : applyWeights rest reservedWords -- Se a palavra não for uma palavra reservada, retorna a tupla normal concatenada com outras iterações da função

-- Recebe a lista de palavras únicas, a lista completa de palavras, e retorna as frequências
countAllWords :: [String] -> [String] -> [(String, Int)]
countAllWords [] _ = [] -- Se a lista de palavras únicas for nula, retorna uma lista nula
countAllWords (uniqueWord : restUnique) fullList =
  (uniqueWord, countWords uniqueWord fullList) : countAllWords restUnique fullList -- Retorna uma lista com uma tupla contendo a palavra única e a contagem concatenada com outras iterações da função.

-- Função para contar quantas vezes a palavra aparece em uma lista
countWords :: String -> [String] -> Int
countWords str list
  | null list = 0 -- Se a String for nula, retorna a tupla com a string e 0
  | head list == str = 1 + countWords str (tail list) -- Se a "cabeça" da lista for a string, soma 1 ao segundo elemento da tupla
  | otherwise = countWords str (tail list) -- Se a "cabeça" da lista for a string, retorna a tupla normal

-- Função para verificar se o caractere está presente em uma lista de caracteres/string
isInString :: String -> Char -> Bool
isInString str c
  | null str = False -- Se a string for nula, retorna False
  | head str == c = True -- Se a "cabeça" da string for o caractere, retorna True
  | otherwise = isInString (tail str) c -- Se a cabeça da string não for o caractere, aplica o procedimento para o resto da string

-- Função auxiliar simples para verificar se uma palavra está na lista de reservadas
isReservedWord :: String -> [String] -> Bool
isReservedWord _ [] = False -- Se a lista de palavras reservadas for nula, retorna False
isReservedWord word (r : rs)
  | word == r = True -- Se a palavra for igual à cabeça da lista de palavras reservadas, retorna True
  | otherwise = isReservedWord word rs -- Se a palavra não for igual à cabeça da lista de palavras reservadas, aplica o procedimento para o resto da lista

-- Função para ler as palavras reservadas do arquivo e retornar uma lista de palavras
listsReservedWords :: IO [String]
listsReservedWords = do
  reservedWords <- readFile nameFileRes -- Lê as palavras reservadas do arquivo
  let listWords = words reservedWords -- Separa as palavras reservadas em uma lista
  return listWords

-- Função que recebe a string de separadores e a string do texto do código
turnSeparatorsIntoSpace :: String -> String -> String
turnSeparatorsIntoSpace _ [] = [] -- Se a string do código for nula, retorna uma string nula
turnSeparatorsIntoSpace separators (c : rest)
  | isInString separators c = ' ' : turnSeparatorsIntoSpace separators rest
  | otherwise = c : turnSeparatorsIntoSpace separators rest

-- Função main
main :: IO ()
main = do
  -- 1. Leitura de todos os arquivos de entrada
  separators <- readFile nameFileSep
  reservedWordsList <- listsReservedWords
  file1 <- readFile nameFileC1
  file2 <- readFile nameFileC2

  -- 2. Limpeza dos separadores
  let stringFile1 = turnSeparatorsIntoSpace separators file1
  let stringFile2 = turnSeparatorsIntoSpace separators file2

  -- 3. Tokenização
  let listWordsFile1 = words stringFile1
  let listWordsFile2 = words stringFile2

  -- 4. Extração de palavras únicas usando 'nub' do Data.List
  let uniqueWordsFile1 = nub listWordsFile1
  let uniqueWordsFile2 = nub listWordsFile2

  -- 5. Contagem bruta de todas as palavras
  let rawCountsFile1 = countAllWords uniqueWordsFile1 listWordsFile1
  let rawCountsFile2 = countAllWords uniqueWordsFile2 listWordsFile2

  -- 6. Aplicação do peso (Frequências finais f1 e f2)
  let finalCountFile1 = applyWeights rawCountsFile1 reservedWordsList
  let finalCountFile2 = applyWeights rawCountsFile2 reservedWordsList

  putStrLn "Frequências f1 (Arquivo 1):"
  print finalCountFile1
  putStrLn "\nFrequências f2 (Arquivo 2):"
  print finalCountFile2
