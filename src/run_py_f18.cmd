cd C:\dev\ziher\ziher_mono\ziher\src\bazel-bin\test

copy /Y f18ziherpy.dll f18ziherpy.pyd

python -c "callback = lambda x,y: x+y+3;import f18ziherpy;f18ziherpy.vminit();f18ziherpy.run('MAIN0',1,0);f18ziherpy.run('MAIN',0,1);quit()"


REM python -c "callback = lambda x,y: x+y+3;import f18ziherpy;f18ziherpy.vminit();f18ziherpy.run('MAIN',1,1);quit()"

cd C:\dev\ziher\ziher_mono\ziher\src
