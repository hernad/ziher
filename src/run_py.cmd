cd C:\dev\ziher\ziher_mono\ziher\src\bazel-bin\test
python -c "callback = lambda x,y: x+y+3;import f18ziherpy;f18ziherpy.vminit();f18ziherpy.set_callback(callback);ret=f18ziherpy.hash('ZIHER_TO_PY',1,1);print(ret);callback2 = lambda x,y: x+y+23;f18ziherpy.set_callback(callback2);print(f18ziherpy.hash('ZIHER_TO_PY',1,1));quit()"

cd C:\dev\ziher\ziher_mono\ziher\src
