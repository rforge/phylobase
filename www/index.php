

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
<li>To install the current release version use <a href="http://cran.r-project.org/web/packages/phylobase/">CRAN</a></li>
<li>To install the <strong>nightly</strong> snapshot (on a good day, when they build properly) go to <a href="http://r-forge.r-project.org/R/?group_id=111"> the <tt>phylobase</tt> build page.</a></li>
<li>The latest version of the source code is always available via <a href="http://subversion.tigris.org/">Subversion</a>:<br/><tt>svn checkout svn://svn.r-forge.r-project.org/svnroot/phylobase</tt></li>
</ul>

<h2>Where can I find help?</h2>

<tt>phylobase</tt> comes with two vignettes that will help you to get started:

<ul>
<li><a href="http://cran.r-project.org/web/packages/phylobase/vignettes/phylobase.pdf">phylo4: classes and methods for phylogenetic trees and data [pdf]</a>
gives an overview of the tree manipulation and plotting functions available in phylobase.</li>

<li><a href="http://cran.r-project.org/web/packages/phylobase/vignettes/developer.pdf">A developers guide for contributing to phylobase [pdf]</a>
provides information about the organization of the package and how to use Subversion.</a>
</ul>

You can contact us on our <a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/phylobase-devl">mailing list</a>.
Alternatively, you can send an email the special interest group mailing list on phylogenetic and comparative methods and analyses: <a href="https://stat.ethz.ch/mailman/listinfo/r-sig-phylo">R-sig-phylo</a>.

<h2>Additional information</h2>
<ul>
<li>The <strong>project summary page</strong> can found <a href="http://r-forge.r-project.org/projects/phylobase/"><strong>here.</strong></a></li>

<li>Much of the plotting code was written as part of the 2008 Google Summer of Code <br/> <a href= "https://www.nescent.org/wg_phyloinformatics/PhyloSoC:Tree_and_data_plotting_in_the_phylobase_project" title = "GSOC"><strong>project description and progress can be found here.</strong></a></li>
</ul>

<img src="geospiza.png">

</body>
</html>
