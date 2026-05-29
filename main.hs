import System.IO


main::IO()

type ReservedWordCount = (String, Int) -- Palavra reservada e contagem de quantas vezes ela aparece

-- Caminho do arquivo de entrada para as palavras reservadas
nameFileRes::FilePath
nameFileRes = "res.txt"

-- Caminho do arquivo de entrada para os separadores
nameFileSep::FilePath
nameFileSep = "sep.txt"

-- Caminho do arquivo de entrada para o primeiro programa
nameFileC1::FilePath
nameFileC1 = "c1.txt"

-- Caminho do arquivo de entrada para o segundo programa
nameFileC2::FilePath
nameFileC2 = "c2.txt"

-- Função para verificar se o caractere está presente em uma lista de caracteres/string
isInString::String -> Char -> Bool
isInString str c
    | (null str) = False -- Se a string for nula, retorna False
    | (head str) == c = True -- Se a "cabeça" da string for o caractere, retorna True
    | otherwise = isInString (tail str) c -- Se a cabeça da string não for o caractere, aplica o procedimento para o resto da string

-- Função para remover os separadores de uma lista de caracteres/string e transformá-los em espaços
turnSeparatorsIntoSpace::String -> IO String
turnSeparatorsIntoSpace str = do
    separators <- readFile nameFileSep -- Lê os separadores do arquivo
    if (null str) then return [] -- Se a string for vazia, retorna a string vazia
    else if (isInString separators (head str)) then do
        -- Se a "cabeça" da string for um separador, substitui por espaço e aplica o procedimento para o resto
        rest <- turnSeparatorsIntoSpace (tail str)
        return (' ':rest)
    else do
        -- Se a "cabeça" da string não for um separador, mantém o caractere e aplica o procedimento para o resto
        rest <- turnSeparatorsIntoSpace (tail str)
        return ((head str):rest)

-- Função para ler as palavras reservadas do arquivo e retornar uma lista de palavras
-- (Ela deve ser aplicada após a função turnSeparatorsIntoSpace. Assim, os separadores serão substituídos por espaços e as palavras reservadas serão separadas corretamente)
listsReservedWords::IO [String]
listsReservedWords = do
    reservedWords <- readFile nameFileRes -- Lê as palavras reservadas do arquivo
    let listWords = words reservedWords -- Separa as palavras reservadas em uma lista
    return listWords
    
-- Função para verificar se o caractere é separador
isSeparator::Char -> IO Bool
isSeparator c = do
    separators <- readFile nameFileSep
    return (isInString separators c)

-- Função para contar quantas vezes a palavra aparece em uma lista
countWords::String -> [String] -> Int
countWords str list 
    | (null list) = 0 -- Se a String for nula, retorna a tupla com a string e 0
    | (head list) == str = 1 + (countWords str (tail list)) -- Se a "cabeça" da lista for a string, soma 1 ao segundo elemento da tupla
    | otherwise = countWords str (tail list) -- Se a "cabeça" da lista for a string, retorna a tupla normal

-- Função para contar quantas vezes cada palavra aparece em uma lista de palavras e retorna uma lista de tuplas com a palavra e a contagem
countWordsInList::[String] -> [String] -> [ReservedWordCount]
countWordsInList resWords wordList
    | (null resWords) || (null wordList) = [] -- Se as listas forem vazias, retorna uma lista vazia
    | otherwise = [(reservedWord, countWords reservedWord wordList)] ++ countWordsInList otherReservedWords wordList -- Retorna uma lista com uma tupla contendo a palavra reservada e a contagem concatenada com outras iterações da função.
    where
        reservedWord = head resWords -- Palavra reservada a ser verificada
        otherReservedWords = tail resWords -- Outras palavras reservadas a serem verificadas
        otherWords = tail wordList -- Outras palavras da lista de palavras

-- Função main
main = do
    putStrLn "Palavras reservadas lidas:"
    reservedWords <- readFile nameFileRes
    file1 <- readFile nameFileC1
    file2 <- readFile nameFileC2

    stringFile1 <- turnSeparatorsIntoSpace file1
    stringFile2 <- turnSeparatorsIntoSpace file2
    
    listWordsFile1 <- return (words stringFile1)
    listWordsFile2 <- return (words stringFile2)

    reservedWordsList <- listsReservedWords

    let countFile1 = countWordsInList reservedWordsList listWordsFile1
    let countFile2 = countWordsInList reservedWordsList listWordsFile2

    putStrLn "Contagem de palavras reservadas no arquivo 1:"
    print countFile1
    putStrLn "Contagem de palavras reservadas no arquivo 2:"
    print countFile2
    