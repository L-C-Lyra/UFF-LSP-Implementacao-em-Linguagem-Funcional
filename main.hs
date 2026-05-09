import System.IO


main::IO()

-- Nome do arquivo de entrada para as palavras reservadas
nomeArquivoRes::String
nomeArquivoRes = "res.txt"

-- Nome do arquivo de entrada para os separadores
nomeArquivoSep::String
nomeArquivoSep = "sep.txt"

-- Nome do arquivo de entrada para o primeiro programa
nomeArquivoC1::String
nomeArquivoC1 = "c1.txt"

-- Nome do arquivo de entrada para o segundo programa
nomeArquivoC2::String
nomeArquivoC2 = "c2.txt"

-- Função para ler o conteúdo de um arquivo e retornar como uma lista de strings
lePalavrasArquivo::String -> IO [String]
lePalavrasArquivo nomeArquivo = do
    conteudo <- readFile nomeArquivo
    return (words conteudo)

-- Função main
main = do
    putStrLn "Palavras reservadas lidas:"
    palavrasReservadas <- lePalavrasArquivo nomeArquivoRes
    print palavrasReservadas