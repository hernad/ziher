# https://docs.bazel.build/versions/2.0.0/skylark/tutorial-creating-a-macro.html

def cmd_bison(name, src, out, **kwargs):
  """Create preprocessed ziher.

  bisonin: ziher_comp.y -> out: ziher_comp.y.c
  """ 
  native.genrule(
    name = name,
    srcs = [src],
    outs = [out + ".c", out + ".h"],
    # https://stackoverflow.com/questions/51073133/bazel-genrule-that-outputs-an-directory
    # https://docs.bazel.build/versions/2.0.0/be/make-variables.html
    # $< - src (one) file
    # $@ - out (one) file
    cmd = "bison $< -o$(execpath " + out + ".c) --defines=$(execpath " + out + ".h)" ,
    **kwargs
  )
