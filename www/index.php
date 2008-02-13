

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

The phylobase package seeks to provide a set of S4 classes and methods for representing and manipulating phylogenetic trees and data in R.

<!-- end of project description -->

<h2>Package resources</h2>
<ul>
<li>To install the current release version (0.3, rev. 104): download one of
the following and install as appropriate for your platform.
(<strong>phylobase does not currently build on 64-bit *nix.
We are working to fix this.  Please contact us if you need
phylobase for this platform, as it may encourage us to work harder.</strong>)
(The current version requires ape version 2.1: MacOS users will
need to specify <tt>install.packages("ape",type="source")</tt>,
and have development tools installed,
to get the right version.)
<ul>
<li><a href="misc/phylobase_0.3.tar.gz">Source</a> (for Linux, Unix, or MacOS/Windows with development
tools installed)</a></li>
<li><a href="misc/phylobase_0.3.zip">Windows binary</a></li>
<li><strong>add MacOS binary here</strong>
</ul></li>
<li>To install the current nightly snapshot (on a good day):
<ul>
<li>Windows or *n*x users: <tt>install.packages("phylobase",repos="http://r-forge.r-project.org")</tt></li>
<li>MacOS users: <tt>install.packages("phylobase", repos="http://r-forge.r-project.org", type = 'source')</tt> (because R-forge does not build MacOS binaries: 
this will also work for Linux/Unix, where <tt>type='source'</tt> is the
default)</li>
</ul>
</li>
<li>A current copy (0.3r104) of the package
<a href="misc/phylobase.pdf">vignette</a></li>
<li>The <strong>project summary page</strong> can found <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </li>
<li>The recent TODO list can be found  <a href= "todo.html" title = "TODO"><strong>here</strong></a>. </li>
</ul>

</body>
</html>
