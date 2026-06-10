\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{xmumthesis}
\usepackage{amsthm}
 
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ProcessOptions\relax
\LoadClass[a4paper]{report}
 


\RequirePackage{graphicx} 
    

\raggedbottom

 
\def\logo{{\includegraphics[width=1.0\linewidth]{xmumlogo}}}

%
% Define text area of page and margin offsets
%
\setlength{\topmargin}{0.0in}
\setlength{\oddsidemargin}{0.33in}
\setlength{\evensidemargin}{-0.08in}
\setlength{\textheight}{9.0in}
\setlength{\textwidth}{6.0in}

%
% Environments
%

% This macro define an environment for front matter that is always
% single column even in a double-column document.

\newenvironment{alwayssingle}{%
       \@restonecolfalse
       \if@twocolumn\@restonecoltrue\onecolumn
       \else\if@openright\cleardoublepage\else\clearpage\fi
       \fi}%
       {\if@restonecol\twocolumn
       \else\newpage\thispagestyle{empty}\fi}

%define title page layout

\renewcommand{\maketitle}{%
\begin{alwayssingle}
    \renewcommand{\footnotesize}{\small}
    \renewcommand{\footnoterule}{\relax}
    \thispagestyle{empty}
  \null\vfill

\begin{center}
 % line1: logo
 \vspace{-10mm}
 {\LARGE {\logo \par} \vspace*{3mm}}
 %{\large \vspace*{40mm} {\logo \par} \vspace*{25mm}}
 % line2: final year project report
 {\large { FINAL YEAR PROJECT REPORT  \par} \vspace*{20mm}}
 % line3: title. The @title is in thesis.tex
 { \Large {\bfseries {\@title}} \vspace{2cm}\par}
\end{center}

\vspace{5mm}

\begin{tabular}{lll}
% line4: author. The @author is in thesis.tex
NAME OF STUDENT &   : &  \studentnameupper
\medskip
\\
STUDENT ID &   : &   \studentid
\medskip
\\
SCHOOL/FACULTY &  : &  DEPARTMENT OF MATHEMATICS       
\medskip
\\
PROGRAMME&  : & MATHEMATICS AND APPLIED MATHEMATICS
\medskip
\\
INTAKE &  : &   \intake
\medskip
\\
\newline
\\
SUPERVISOR & : & \supervisorupper
\end{tabular}

\vspace{40mm}

\begin{center}
{\large \thesismonth  \,\,\thesisyear}
\end{center}

\null\vfill
\end{alwayssingle}}

 

\newenvironment{coverpage}{\begin{alwayssingle}
 \thispagestyle{empty}
\pagecolor{darkblue}
\begin{quote}}
{\end{quote}
\end{alwayssingle}}

\newenvironment{declaration}
{\begin{alwayssingle} \thispagestyle{plain}
\begin{center}
%\vspace*{5mm}
{\large \bfseries DECLARATION}
\end{center}
\vspace*{30mm}
\begin{quote}}
{\end{quote}\end{alwayssingle}}

 
\newenvironment{approval}
{\begin{alwayssingle} \thispagestyle{plain}
\begin{center}
%\vspace*{5mm}
{\large \bfseries APPROVAL FOR SUBMISSION}
\end{center}
\vspace*{20mm}
\begin{quote}}
{\end{quote}\end{alwayssingle}}

 
\newenvironment{copyrightstatement}
{\begin{alwayssingle} \thispagestyle{plain}
\vspace*{65mm}
\begin{quote}}
{\end{quote}\end{alwayssingle}}


 
\newenvironment{acknowledgements}
{\begin{alwayssingle} \thispagestyle{plain}
\begin{center}
%\vspace*{5mm}
{\large \bfseries ACKNOWLEDGEMENTS}
\end{center}
\vspace{20mm}
\begin{quote}}
{\end{quote}\end{alwayssingle}}


 
\renewenvironment{abstract}
{\begin{alwayssingle} \thispagestyle{plain}
 \begin{center}
 %\vspace*{2.5cm}
 {\large \bfseries  ABSTRACT}
 \end{center}
 \vspace{5mm}
 \begin{quote}}
{\end{quote}\end{alwayssingle}}


 

\newenvironment{romanpages}
{\cleardoublepage\setcounter{page}{1}\renewcommand{\thepage}{\roman{page}}}
{\cleardoublepage\renewcommand{\thepage}{\arabic{page}}\setcounter{page}{1}}


 
\usepackage[ ]{titlesec}
\usepackage{color}
\definecolor{RoyalRed}{RGB}{157,16, 45}
\titleformat{\chapter}[display]
{ \centering\bfseries \large   \color{black}}
{\centering   \MakeUppercase   { \chaptertitlename } {\MakeUppercase \thechapter }} {0 pt}{\MakeUppercase}  
\titlespacing{\chapter}{0pt}{-25pt}{25.5pt}
\titlespacing{name=\chapter, numberless}{0pt}{16pt}{15pt}
\titleformat{\section}
  {\normalfont\fontsize{12}{15}\bfseries}{\thesection}{1em}{}
\titleformat{\subsection}
  {\normalfont\fontsize{12}{15}\bfseries}{\thesubsection}{1em}{}

 
 
\setlength\parindent{0pt}

 
\theoremstyle{definition}
\newtheorem{theorem}{Theorem}[chapter]
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{corollary}[theorem]{Corollary}
\newtheorem{definition}[theorem]{Definition}
\newtheorem{example}[theorem]{Example}
\newtheorem{conjecture}[theorem]{Conjecture}
\newtheorem{xca}[theorem]{Exercise}
\newtheorem{remark}[theorem]{Remark}

\renewcommand{\bibname}{\large REFERENCES}

