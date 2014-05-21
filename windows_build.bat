@echo off
echo This file tries to setup a windows machine for CSPLib.
echo You should have already installed python 3.4 from www.python.org

if exist c:\Python34\Scripts\pip.exe (
   echo Found python 3.4 installed in the usual place
   set pipcmd=c:\Python34\Scripts\pip.exe
   set pycmd=c:\Python34\python.exe
   goto pyfound
)

pip.exe 2> NUL 1> NUL

if not %ERRORLEVEL%=9009 (
   echo Found pip in the path...
   python.exe -V 2> NUL 1> NUL
   if not %ERRORLEVEL%==9009 (
      echo and python as well!
      echo I will hope this is python 3.4, and use it!
      set pipcmd=pip.exe
      set pycmd=python.exe
      goto pyfound
  )
)
echo Cannot find python...
echo Please run the following yourself:
echo pip install -r scripts/support/packages.txt
echo pip install --allow-external guess-language-spirit --allow-unverified guess-language-spirit mdx_smartypants==1.5.1 --pre
echo python scripts/framework/generate_web_site.py
exit 1

:pyfound

echo Installing packages (this may take up to 5 minutes first time)
%pipcmd% install -q -r scripts/support/packages.txt
%pipcmd% install -q --allow-external guess-language-spirit --allow-unverified guess-language-spirit mdx_smartypants==1.5.1 --pre
echo Building csplib website
%pycmd% scripts/framework/generate_web_site.py
echo Website in _deploy directory
