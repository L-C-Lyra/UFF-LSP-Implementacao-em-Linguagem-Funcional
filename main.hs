import System.IO


main::IO()

-- Nome do arquivo de entrada para as palavras reservadas
nameFileRes::String
nameFileRes = "res.txt"

-- Nome do arquivo de entrada para os separadores
nameFileSep::String
nameFileSep = "sep.txt"

-- Nome do arquivo de entrada para o primeiro programa
nameFileC1::String
nameFileC1 = "c1.txt"

-- Nome do arquivo de entrada para o segundo programa
nameFileC2::String
nameFileC2 = "c2.txt"

-- Função para ler o conteúdo de um arquivo e retornar como uma lista de strings
readWordsFile::String -> IO [String]
readWordsFile fileName = do
    content <- readFile fileName
    return (words content)

-- Função para verificar se uma palavra está presente em uma lista de palavras
isInList::String -> [String] -> Bool

-- Função para verificar se uma palavra é uma palavra reservada
isReservedWord::String -> [String] -> Bool
isReservedWord word reservedWords = isInList word reservedWords

-- Função para verificar se uma palavra é um separador
isSeparator::String -> [String] -> Bool
isSeparator word separators = isInList word separators

-- Função main
main = do
    putStrLn "Palavras reservadas lidas:"
    reservedWords <- readWordsFile nameFileRes
    print reservedWords