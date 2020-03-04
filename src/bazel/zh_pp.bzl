# https://docs.bazel.build/versions/2.0.0/skylark/tutorial-creating-a-macro.html

#def as_path(p, is_windows):
#    if is_windows:
#        return p.replace("/", "\\")
#    else:
#        return p

def zh_pp(name, ppin, out, args="-izh_zero", **kwargs):
  """Create preprocessed ziher.


  std_gen.zhh -> std_gen.zhh.zpp
  """ 
  native.genrule(
    name = name,
    srcs = ["//zh_pp/main:zhpp", "//zh_zero:headers", ppin],
    outs = [out],
    # https://stackoverflow.com/questions/51073133/bazel-genrule-that-outputs-an-directory
    # https://docs.bazel.build/versions/2.0.0/be/make-variables.html
    # $< - src (one) file
    # $@ - out (one) file
    cmd = "$(location //zh_pp/main:zhpp) zh_pp/" + ppin + " " + args + " -o$@",
    **kwargs
  )

