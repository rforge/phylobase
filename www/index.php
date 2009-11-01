

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

<h2>How to install phylobase?</h2>

<h3>Current release</h3>

<ul>
<li>You can download the current release version from <a href="http://cran.r-project.org/web/packages/phylobase/index.html">R-forge</a></li>
<li>Alternatively, you can install it directly within R: <tt>install.packages("phylobase")</tt><li>
</ul>

<h3>Nightly snapshot</h3>

<ul>
<li>You can download the <a href="https://r-forge.r-project.org/R/?group_id=111">nighthly</a> snapshot (on a good day) from R-forge.</li>
<li>You can also install it directly within R: <tt>install.packages("phylobase", repos="http://r-forge.r-project.org")</tt>
</ul>

<h3>SVN access</h3>

You can checkout the latest version of the source code using <a href="http://subversion.tigris.org/">Subversion</a>:<br/>
<tt>svn checkout svn://svn.r-forge.r-project.org/svnroot/phylobase</tt>


<h2>Where can I find help?</h2>

<tt>phylobase</tt> comes with two vignettes that will help you to get started:

<ul>
<li><a href="http://cran.r-project.org/web/packages/phylobase/vignettes/phylobase.pdf">phylo4: classes and methods for phylogenetic trees and data</a>
gives an overview of the tree manipulation and plotting functions available in phylobase</li>
<li><a href="http://cran.r-project.org/web/packages/phylobase/vignettes/developer.pdf">A developers guide for contributing to phylobase</a>
provides information about the organization of the package and how to use Subversion.</a>
</ul>

The best way to get in touch with us is to send an email to our <a href="http://r-forge.r-project.org/mail/?group_id=111">mailing list</a>.
If you are lucky you can also find us on the <tt>#phylobase</tt> IRC channel at freenode.

<h2>Misc</h2>
<ul>
<li>Peter's 2008 Google Summer of Code <a href="https://www.nescent.org/wg_phyloinformatics/PhyloSoC:Tree_and_data_plotting_in_the_phylobase_project" title="GSOC">project description and progress</a>.</li>
</ul>

</body>
</html>
