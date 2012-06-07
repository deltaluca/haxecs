Idea is to create a translator from a subset of Haxe to C#.

Purpose being to provide a native C# port of Nape akin to the native .swc's available for AS3 use (Which also required some post-build modifications to the Haxe produced swc's to keep the API consistent)

Whilst Haxe now targets C# (in nightlies at time of writing). The output is not suitable for use in a native C# project where the need to support the entirety of the Haxe language bastardises the output to a significant degree.

Another key note is to restrict the subset of Haxe supported so that preprocessor information can be retained in the translation (So that w.r.t to Nape a single C# version can be produced instead of 3 seperate versions (for build modes) as is required for .swc targets in AS3)