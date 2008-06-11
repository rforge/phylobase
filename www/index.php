

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
	<link href="http://r-forge.r-project.org/themes/rforge/styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>

<!-- own website starts here, the following may be changed as you like -->

The phylobase package seeks to provide a set of S4 classes and methods for representing and manipulating phylogenetic trees and data in R.

<!-- end of project description -->

<h2>Package resources</h2>
<ul>
<li>To install the current release version (0.3, rev. 104): download one of
the following and install as appropriate for your platform.</li>
<li>The current version requires ape version 2.1: MacOS users will
need to specify <tt>install.packages("ape",type="source")</tt>,
and have development tools installed,
to get the right version.)</li>
</ul>

<ul>
<li><a href="misc/phylobase_0.3.tar.gz">Source</a> (for Linux, Unix, or MacOS/Windows with development
tools installed)</li>
<li><a href="misc/phylobase_0.3.zip">Windows binary</a></li>
<li><a href="misc/phylobase_0.3_R_i386-apple-darwin8.10.1.tar.gz">MacOS <strong>Intel</strong> binary</a> If you require a PowerPC binary please ask on the developer list.</li>
</ul>

To install the <strong>nightly</strong> snapshot (on a good day):

<ul>
<li>Windows or *nix users: <tt>install.packages("phylobase", repos="http://r-forge.r-project.org")</tt></li>
<li>MacOS users: <tt>install.packages("phylobase", repos="http://r-forge.r-project.org", type = 'source')</tt></li>
</ul>

Additional information:
<ul>
<li>A current copy (0.3r104) of the package
<a href="misc/phylobase.pdf"><strong>vignette</strong></a></li>
<li>The <strong>project summary page</strong> can found <a href="http://r-forge.r-project.org/projects/phylobase/"><strong>here</strong></a>. </li>
<li>Google Summer of Code <a href= "gsoc.html" title = "GSOC"><strong>project description and progress</strong></a>. </li>
</ul>

The phylobase email lists can be accessed and subscribed to from <a href="http://r-forge.r-project.org/mail/?group_id=111">this page.</a>

</body>
</html>
