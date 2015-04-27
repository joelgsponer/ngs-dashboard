<h1>Help</h1>

<h4>Data input</h4>
Fill out the provided form and do not forget to save after your entered your data.

<h4>Data storage</h4>
All of the data is stored on the Workhorse computer. Specifically in a <a href="https://www.sqlite.org/">SQLlite database</a> called "ngs.sqlite". The database can be found under the path: <code>/var/shiny-server/www/webCGH/apps/ngs/data/</code> along with the vcf (in xls format) and coverage files. SQLlite databses can be read with various programs including Microsoft Access and Excel, please refer to the specific manuals for further information.

<h4>Trouble shooting</h4>
<ol>
<li>Refresh the page and retry. If this does not work refer to 2.</li>
<li>On the workhorse machine in the commandline type <code>sudo service shiny-server restart</code> and refresh the page.</li>
<li>If this still does not work contact me:<a href="mailto:jgsponer@gmail.com">Joel Gsponer</a></li>
</ol>

<h4>Technical Information</h4>
The whole application is written in R, Specifically using the shiny package (<a href="http://shiny.rstudio.com/">for more information click here</a>). This creates kind of a Node.js based webserver which is capable of broadcasting the application on the network to be accessible via a browser to a client (depending on the version it does not work with Internet Explorer!). The server can be accessed via ip-adress:port (e.g. <a href="http://workhorse.dyn.uhbs.ch:3838">workhorse.dyn.uhbs.ch:3838</a>)
