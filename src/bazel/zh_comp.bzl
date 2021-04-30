# https://docs.bazel.build/versions/2.0.0/skylark/tutorial-creating-a-macro.html

def zh_comp(name, zhin, args="-n -izh_zero", **kwargs):
  """Create preprocessed ziher.

  
  """ 
  native.genrule(
    name = name,
    srcs = [
      "//zh_comp/main:zhcomp", 
      "//zh_zero:headers_filegroup", 
      "//zh_rtl:headers_filegroup", 
      zhin
    ],
    outs = [zhin + ".c"],
    # $< - src (one) file
    # $@ - out (one) file
    cmd = "$(location //zh_comp/main:zhcomp) $(execpath " + zhin + ")" + " " + args + " -o$(execpath " + zhin + ".c)",
    **kwargs
  )


#def zh_comp_all(name, srcs, args = "-n -izh_zero", **kwargs):
#  """Compile all ziher sources
#
#  """
#  outs = []
#  for src in srcs:
#    zh_comp(
#         name = "zh_%s" % src,
#         zhin = src,
#         args = args
#    )
#    outs.append(src + ".c")
#  
#  return outs
#  


#https://github.com/bazelbuild/bazel/blob/master/site/docs/skylark/rules-tutorial.md


def _zh_comp_impl(ctx):
  
    #args = [ctx.outputs.out.path] + [f.path for f in ctx.files.srcs]
    outs = []
    for hdr in ctx.files.hdrs:
      outs.append(hdr)
    for src in ctx.files.srcs:
      out = ctx.actions.declare_file(src.path + ".c")
      outs.append(out)
      args = [ src.path, "-o" + out.path ]
      args += ctx.attr.args
      inputs = []
      inputs += ctx.files.deps 
      inputs.append(src)
      #args = [ "." ]
      ctx.actions.run(
        inputs = inputs,
        outputs = [out] ,
        arguments = args,
        progress_message = "ZH>> ziher compiling: " + "  ".join(args),
        #executable = "find"
        executable = ctx.executable.zh_comp,
      )

    return [DefaultInfo(files = depset(outs))]


zh_comp_all = rule(
    implementation = _zh_comp_impl,
    attrs = {
        "srcs": attr.label_list(
          allow_files = [".zh", ".zhh"]
        ),
        "hdrs": attr.label_list(
          allow_files = [".json", ".txt", ".xml"]
        ),
        "args": attr.string_list(
          default = []
        ),
        # Dependency attributes, such as attr.label and attr.label_list, 
        # declare a dependency from the target that owns the attribute 
        # to the target whose label appears in the attribute’s value. 
        # This kind of attribute forms the basis of the target graph.
        "deps": attr.label_list(
        ),
        "zh_comp": attr.label(
            executable = True,
            cfg = "host",
            allow_files = True,
            default = Label("//zh_comp/main:zhcomp"),
        ),
        "outs": attr.output_list(
            doc = "A list of generated c files ",
        ),
    }
    #https://github.com/bazelbuild/examples/blob/master/rules/implicit_output/hash.bzl

)
