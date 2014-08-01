\documentclass{beamer}
%include polycode.fmt
\usepackage{fancyvrb}
\usepackage{listings}
\lstset{language=Haskell}
\usecolortheme{wolverine}
\begin{document}
\title{Extensible Effects \\ A Library for Managing Side-Effects}   
\author{Justin Bailey \\ Presented by the Functional Programming Guild}
\date{\today} 
\frame{\titlepage} 

\section{Motivation}
\begin{frame}[fragile]
  \frametitle{Side Effects}

  What does this function do?

  \begin{onlyenv}<1>
    \begin{Verbatim}
      // A Java method
      public static int add(int a, int b) {
        ...
      }
    \end{Verbatim}
  \end{onlyenv}

  \begin{onlyenv}<2>
    \begin{Verbatim}
      // A Java method
      public static int add(int a, int b) {
        System.exit(0);
      }
    \end{Verbatim}
  \end{onlyenv}
\end{frame}

\begin{frame}[fragile]
  \frametitle{EXPLICIT Side Effects}
Use |IO| to represent effects: 

< getChar :: Handle -> IO Char
< openFile :: FilePath -> IO Handle
< putChar :: Handle -> Char -> IO ()

\end{frame}

\begin{frame}
  \frametitle{EXPLICIT Side Effects}

\begin{onlyenv}<1>
What does this function do?

< add :: Int -> Int -> Int
< add a b = ...

\emph{Hint:\ } |exitFailure :: IO ()|
\end{onlyenv}

\begin{onlyenv}<2>
Possible?

< add :: Int -> Int -> Int
< add a b = exitWith (ExitFailure 0) 

\emph{Hint:\ } |exitFailure :: IO ()|
\end{onlyenv}

\begin{onlyenv}<3>
|IO () /= Int|

< add :: Int -> Int -> Int
< add a b = exitWith (ExitFailure 0) :: IO () -- no
\end{onlyenv}

\begin{onlyenv}<4>
One \emph{correct} possibility:

< add :: Int -> Int -> Int
< add a b = a + b -- Int
\end{onlyenv}

\begin{onlyenv}<5>
Types don't solve \emph{everything} ...
< add :: Int -> Int -> Int
< add a b = add a b
\end{onlyenv}

\end{frame}

\begin{frame}
\frametitle{|IO|: The ``Sin'' Bin}

< putStrLn :: String -> IO ()
< openFile :: FilePath -> FileMode -> IO Handle
< exitFailure :: IO ()
< exitSuccess :: IO ()
< forkIO :: IO () -> IO ThreadId
\end{frame}

\begin{frame}[fragile]
\frametitle{|IO|: The ``Sin'' Bin}
\begin{onlyenv}<1>
What does this function do?

< add a b :: Int -> Int -> IO Int
< add a b = ...
\end{onlyenv}

\begin{onlyenv}<2>
Valid:

< add a b :: Int -> Int -> IO Int
< add a b = return (a + b)
\end{onlyenv}

\begin{onlyenv}<3>
Valid:

< add a b :: Int -> Int -> IO Int
< add a b = do
<   exitFailure
<   return 0
\end{onlyenv}

\begin{onlyenv}<4>
Valid:

< add a b :: Int -> Int -> IO Int
< add a b = do
<   deleteAllFiles 
<   return (a + b)
\end{onlyenv}
\end{frame}

\section{Effects}

\begin{frame}
  \frametitle{Break Effects into Smaller Types}

< openFile :: FilePath -> FileMode -> OpenFile Handle
< readAll :: Handle -> ReadFile String
< writeAll :: Handle -> String -> WriteFile ()

\end{frame}

\begin{frame}
  \frametitle{Break Effects into Smaller Types}
  But how do you combine effects?

  \begin{onlyenv}<1>
< cp :: FilePath -> FilePath -> ???
< cp src dest = do
<   s <- openFile src -- OpenFile Handle
<   contents <- readAll s -- ReadFile String
<   d <- openFile dest -- OpenFile Handle
<   writeAll d contents -- WriteFile ()
  \end{onlyenv}

\begin{onlyenv}<2>
< cp :: FilePath -> FilePath -> ???
< cp src dest = do
<   s <- openFile src :: OpenFile Handle
<   contents <- readAll s :: ReadFile String
<   d <- openFile dest :: OpenFile Handle
<   writeAll d contents :: WriteFile ()
  \end{onlyenv}
 
\end{frame}

\begin{frame}
  \frametitle{Extensible Effects}
Make effects a \emph{list}:

< openFile :: FilePath -> FileMode 
<          -> Eff (OpenFile :> effects) Handle
< readAll :: Handle 
<         -> Eff (ReadFile :> effects) String
< writeAll :: Handle -> String 
<          -> Eff (WriteFile :> effects) ()

\begin{onlyenv}<2>
< cp :: FilePath -> FilePath 
<    -> Eff (OpenFile :> ReadFile :> WriteFile :> effects) 
<    -> Eff effects ()
< cp src dest = .. -- almost the same code

\end{onlyenv}
\end{frame}

\begin{frame}
  \frametitle{Example: The |ReadableFile| Effect}
\begin{onlyenv}<1>
Ideally:

< runReadable :: FilePath -> Eff (ReadableFile :> r) -> Eff r result
< getReadableHandle :: (Member ReadableFile r) => Eff r Handle
\end{onlyenv}

\begin{onlyenv}<2>
Actually:

< getReadableHandle :: (Member ReadableFile r) => Eff r Handle
< runReadableFile :: (SetMember Lift (Lift IO) r) => FilePath 
<   -> Eff (ReadableFile :> r) result 
<   -> Eff r result
\end{onlyenv}

\end{frame}

\begin{frame}
  \frametitle{Reading a File}
\begin{onlyenv}<1>
Primitives for reading files:

< readChar :: (Member ReadFile r) => Eff r Char
< atEOF :: (Member ReadFile r) => Eff r Bool
\end{onlyenv}

\begin{onlyenv}<2>
|ReadFile| requires |ReadableFile|:

< runReadFile :: (SetMember Lift (Lift IO) r, Member ReadableFile r) 
<   => Eff (ReadFile :> r) result 
<   -> Eff r result
\end{onlyenv}
\end{frame}

\begin{frame}
  \frametitle{Writing a File}
  \begin{onlyenv}<1>
    |WritableFile| and |WriteFile|:

< getWriteableHandle :: (Member WriteableFile r) => Eff r Handle
< runWriteableFile :: (SetMember Lift (Lift IO) r) => FilePath 
<   -> Eff (WriteableFile :> r) result 
<   -> Eff r result
< writeChar :: (Member WriteFile r) => Char -> Eff r ()
  \end{onlyenv}

  \begin{onlyenv}<2>
    |WriteFile| requires |WriteableFile|:

< runWriteFile :: (SetMember Lift (Lift IO) r, Member WriteableFile r) 
<   => Eff (WriteFile :> r) result 
<   -> Eff r result
    
  \end{onlyenv}
\end{frame}

\begin{frame}[fragile]
  \frametitle{Impossible Programs}
  \begin{onlyenv}<1>
    What does this program do?

< cp_r src dst = runReadableFile src $ runReadFile $ do
<   s <- getReadableHandle 
<   d <- getWriteableHandle
<   contents <- readAll s
<   writeAll d contents
  \end{onlyenv}

  \begin{onlyenv}<2>
Compilation error:

\begin{Verbatim}
 *Main> cp_r "Setup.hs" "foo"

 <interactive>:30:1:
    No instance for (Member (* -> *) * WriteFile r0)
      arising from a use of `cp_r'  
    ...
\end{Verbatim}
  \end{onlyenv}
  
\end{frame}

\begin{frame}
  \frametitle{Copying a File (Safely)}
< cp src dest = runLift $ runWriteableFile dest $ runWriteFile $ 
<   runReadableFile src $ runReadFile $ do
<     h <- readChar
<      writeChar h

\end{frame}

\end{document}
