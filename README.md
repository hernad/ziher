What is `ziher`? Find [here](https://github.com/hernad/ziher/issues).

![ziher](/doc/ziher.jpg?raw=true)


## python

<pre>
C:\dev\ziher\ziher_mono\ziher\src\bazel-bin\test>dir f18ziherpy.pyd
 Volume in drive C has no label.
 Volume Serial Number is F0E2-A75E

 Directory of C:\dev\ziher\ziher_mono\ziher\src\bazel-bin\test

27. 08. 2022.  15:41            16.896 f18ziherpy.pyd
               1 File(s)         16.896 bytes
               0 Dir(s)  762.793.041.920 bytes free
</pre>


<pre>
C:\dev\ziher\ziher_mono\ziher\src\bazel-bin\test>dumpbin /dependents f18ziherpy.pyd

Dump of file f18ziherpy.pyd

File Type: DLL

  Image has the following dependencies:
    ziher.dll
    hello_py.dll
    python3.dll
    VCRUNTIME140.dll
    api-ms-win-crt-stdio-l1-1-0.dll
    api-ms-win-crt-runtime-l1-1-0.dll
    api-ms-win-crt-string-l1-1-0.dll
    KERNEL32.dll

  Summary

        1000 .data
        1000 .pdata
        2000 .rdata
        1000 .reloc
        2000 .text
</pre>


<pre>
C:\dev\ziher\ziher_mono\ziher\src\bazel-bin\test>dumpbin /exports f18ziherpy.pyd
Dump of file f18ziherpy.pyd
File Type: DLL
  Section contains the following exports for f18ziherpy.dll

    00000000 characteristics
    FFFFFFFF time date stamp
        0.00 version
           1 ordinal base
           2 number of functions
           2 number of names

    ordinal hint RVA      name

          1    0 00001720 PyInit_f18ziherpy = PyInit_f18ziherpy
          2    1 00001000 hello_py_ext_101 = hello_py_ext_101

</pre>

## Ziher from python

run parametri: MAIN - ime funkcije, 1 - init console, 1 - release console

<pre>
>>> import f18ziherpy
>>> f18ziherpy.vminit();f18ziherpy.run("MAIN",1,1);f18ziherpy.run("MAIN",1,1);quit()
</pre>


### callback lamda funkcija

Definišemo lamda funkciju x+y+3. Poziv init console=1, release console=1 f18ziherpy.hash('ZIHER_TO_PY',1,1). Hash vraća python dict objekat napunjen unutar ziher kao hash varijabla.

<pre>
python -c "callback = lambda x,y: x+y+3;import f18ziherpy;f18ziherpy.vminit();f18ziherpy.set_callback(callback);ret=f18ziherpy.hash('ZIHER_TO_PY',1,1);print(ret);quit()"
</pre>

dva setovanja callback-a i dva hash poziva

<pre>
python -c "callback = lambda x,y: x+y+3;import f18ziherpy;f18ziherpy.vminit();f18ziherpy.set_callback(callback);ret=f18ziherpy.hash('ZIHER_TO_PY',1,1);print(ret);callback2 = lambda x,y: x+y+23;f18ziherpy.set_callback(callback2);print(f18ziherpy.hash('ZIHER_TO_PY',1,1));quit()"
</pre>
