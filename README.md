<!-- R syntax highlighter -->
<script type="text/javascript">
var hljs=new function(){function m(p){return p.replace(/&/gm,"&amp;").replace(/</gm,"&lt;")}function f(r,q,p){return RegExp(q,"m"+(r.cI?"i":"")+(p?"g":""))}function b(r){for(var p=0;p<r.childNodes.length;p++){var q=r.childNodes[p];if(q.nodeName=="CODE"){return q}if(!(q.nodeType==3&&q.nodeValue.match(/\s+/))){break}}}function h(t,s){var p="";for(var r=0;r<t.childNodes.length;r++){if(t.childNodes[r].nodeType==3){var q=t.childNodes[r].nodeValue;if(s){q=q.replace(/\n/g,"")}p+=q}else{if(t.childNodes[r].nodeName=="BR"){p+="\n"}else{p+=h(t.childNodes[r])}}}if(/MSIE [678]/.test(navigator.userAgent)){p=p.replace(/\r/g,"\n")}return p}function a(s){var r=s.className.split(/\s+/);r=r.concat(s.parentNode.className.split(/\s+/));for(var q=0;q<r.length;q++){var p=r[q].replace(/^language-/,"");if(e[p]){return p}}}function c(q){var p=[];(function(s,t){for(var r=0;r<s.childNodes.length;r++){if(s.childNodes[r].nodeType==3){t+=s.childNodes[r].nodeValue.length}else{if(s.childNodes[r].nodeName=="BR"){t+=1}else{if(s.childNodes[r].nodeType==1){p.push({event:"start",offset:t,node:s.childNodes[r]});t=arguments.callee(s.childNodes[r],t);p.push({event:"stop",offset:t,node:s.childNodes[r]})}}}}return t})(q,0);return p}function k(y,w,x){var q=0;var z="";var s=[];function u(){if(y.length&&w.length){if(y[0].offset!=w[0].offset){return(y[0].offset<w[0].offset)?y:w}else{return w[0].event=="start"?y:w}}else{return y.length?y:w}}function t(D){var A="<"+D.nodeName.toLowerCase();for(var B=0;B<D.attributes.length;B++){var C=D.attributes[B];A+=" "+C.nodeName.toLowerCase();if(C.value!==undefined&&C.value!==false&&C.value!==null){A+='="'+m(C.value)+'"'}}return A+">"}while(y.length||w.length){var v=u().splice(0,1)[0];z+=m(x.substr(q,v.offset-q));q=v.offset;if(v.event=="start"){z+=t(v.node);s.push(v.node)}else{if(v.event=="stop"){var p,r=s.length;do{r--;p=s[r];z+=("</"+p.nodeName.toLowerCase()+">")}while(p!=v.node);s.splice(r,1);while(r<s.length){z+=t(s[r]);r++}}}}return z+m(x.substr(q))}function j(){function q(x,y,v){if(x.compiled){return}var u;var s=[];if(x.k){x.lR=f(y,x.l||hljs.IR,true);for(var w in x.k){if(!x.k.hasOwnProperty(w)){continue}if(x.k[w] instanceof Object){u=x.k[w]}else{u=x.k;w="keyword"}for(var r in u){if(!u.hasOwnProperty(r)){continue}x.k[r]=[w,u[r]];s.push(r)}}}if(!v){if(x.bWK){x.b="\\b("+s.join("|")+")\\s"}x.bR=f(y,x.b?x.b:"\\B|\\b");if(!x.e&&!x.eW){x.e="\\B|\\b"}if(x.e){x.eR=f(y,x.e)}}if(x.i){x.iR=f(y,x.i)}if(x.r===undefined){x.r=1}if(!x.c){x.c=[]}x.compiled=true;for(var t=0;t<x.c.length;t++){if(x.c[t]=="self"){x.c[t]=x}q(x.c[t],y,false)}if(x.starts){q(x.starts,y,false)}}for(var p in e){if(!e.hasOwnProperty(p)){continue}q(e[p].dM,e[p],true)}}function d(B,C){if(!j.called){j();j.called=true}function q(r,M){for(var L=0;L<M.c.length;L++){if((M.c[L].bR.exec(r)||[null])[0]==r){return M.c[L]}}}function v(L,r){if(D[L].e&&D[L].eR.test(r)){return 1}if(D[L].eW){var M=v(L-1,r);return M?M+1:0}return 0}function w(r,L){return L.i&&L.iR.test(r)}function K(N,O){var M=[];for(var L=0;L<N.c.length;L++){M.push(N.c[L].b)}var r=D.length-1;do{if(D[r].e){M.push(D[r].e)}r--}while(D[r+1].eW);if(N.i){M.push(N.i)}return f(O,M.join("|"),true)}function p(M,L){var N=D[D.length-1];if(!N.t){N.t=K(N,E)}N.t.lastIndex=L;var r=N.t.exec(M);return r?[M.substr(L,r.index-L),r[0],false]:[M.substr(L),"",true]}function z(N,r){var L=E.cI?r[0].toLowerCase():r[0];var M=N.k[L];if(M&&M instanceof Array){return M}return false}function F(L,P){L=m(L);if(!P.k){return L}var r="";var O=0;P.lR.lastIndex=0;var M=P.lR.exec(L);while(M){r+=L.substr(O,M.index-O);var N=z(P,M);if(N){x+=N[1];r+='<span class="'+N[0]+'">'+M[0]+"</span>"}else{r+=M[0]}O=P.lR.lastIndex;M=P.lR.exec(L)}return r+L.substr(O,L.length-O)}function J(L,M){if(M.sL&&e[M.sL]){var r=d(M.sL,L);x+=r.keyword_count;return r.value}else{return F(L,M)}}function I(M,r){var L=M.cN?'<span class="'+M.cN+'">':"";if(M.rB){y+=L;M.buffer=""}else{if(M.eB){y+=m(r)+L;M.buffer=""}else{y+=L;M.buffer=r}}D.push(M);A+=M.r}function G(N,M,Q){var R=D[D.length-1];if(Q){y+=J(R.buffer+N,R);return false}var P=q(M,R);if(P){y+=J(R.buffer+N,R);I(P,M);return P.rB}var L=v(D.length-1,M);if(L){var O=R.cN?"</span>":"";if(R.rE){y+=J(R.buffer+N,R)+O}else{if(R.eE){y+=J(R.buffer+N,R)+O+m(M)}else{y+=J(R.buffer+N+M,R)+O}}while(L>1){O=D[D.length-2].cN?"</span>":"";y+=O;L--;D.length--}var r=D[D.length-1];D.length--;D[D.length-1].buffer="";if(r.starts){I(r.starts,"")}return R.rE}if(w(M,R)){throw"Illegal"}}var E=e[B];var D=[E.dM];var A=0;var x=0;var y="";try{var s,u=0;E.dM.buffer="";do{s=p(C,u);var t=G(s[0],s[1],s[2]);u+=s[0].length;if(!t){u+=s[1].length}}while(!s[2]);if(D.length>1){throw"Illegal"}return{r:A,keyword_count:x,value:y}}catch(H){if(H=="Illegal"){return{r:0,keyword_count:0,value:m(C)}}else{throw H}}}function g(t){var p={keyword_count:0,r:0,value:m(t)};var r=p;for(var q in e){if(!e.hasOwnProperty(q)){continue}var s=d(q,t);s.language=q;if(s.keyword_count+s.r>r.keyword_count+r.r){r=s}if(s.keyword_count+s.r>p.keyword_count+p.r){r=p;p=s}}if(r.language){p.second_best=r}return p}function i(r,q,p){if(q){r=r.replace(/^((<[^>]+>|\t)+)/gm,function(t,w,v,u){return w.replace(/\t/g,q)})}if(p){r=r.replace(/\n/g,"<br>")}return r}function n(t,w,r){var x=h(t,r);var v=a(t);var y,s;if(v){y=d(v,x)}else{return}var q=c(t);if(q.length){s=document.createElement("pre");s.innerHTML=y.value;y.value=k(q,c(s),x)}y.value=i(y.value,w,r);var u=t.className;if(!u.match("(\\s|^)(language-)?"+v+"(\\s|$)")){u=u?(u+" "+v):v}if(/MSIE [678]/.test(navigator.userAgent)&&t.tagName=="CODE"&&t.parentNode.tagName=="PRE"){s=t.parentNode;var p=document.createElement("div");p.innerHTML="<pre><code>"+y.value+"</code></pre>";t=p.firstChild.firstChild;p.firstChild.cN=s.cN;s.parentNode.replaceChild(p.firstChild,s)}else{t.innerHTML=y.value}t.className=u;t.result={language:v,kw:y.keyword_count,re:y.r};if(y.second_best){t.second_best={language:y.second_best.language,kw:y.second_best.keyword_count,re:y.second_best.r}}}function o(){if(o.called){return}o.called=true;var r=document.getElementsByTagName("pre");for(var p=0;p<r.length;p++){var q=b(r[p]);if(q){n(q,hljs.tabReplace)}}}function l(){if(window.addEventListener){window.addEventListener("DOMContentLoaded",o,false);window.addEventListener("load",o,false)}else{if(window.attachEvent){window.attachEvent("onload",o)}else{window.onload=o}}}var e={};this.LANGUAGES=e;this.highlight=d;this.highlightAuto=g;this.fixMarkup=i;this.highlightBlock=n;this.initHighlighting=o;this.initHighlightingOnLoad=l;this.IR="[a-zA-Z][a-zA-Z0-9_]*";this.UIR="[a-zA-Z_][a-zA-Z0-9_]*";this.NR="\\b\\d+(\\.\\d+)?";this.CNR="\\b(0[xX][a-fA-F0-9]+|(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)";this.BNR="\\b(0b[01]+)";this.RSR="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|\\.|-|-=|/|/=|:|;|<|<<|<<=|<=|=|==|===|>|>=|>>|>>=|>>>|>>>=|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~";this.ER="(?![\\s\\S])";this.BE={b:"\\\\.",r:0};this.ASM={cN:"string",b:"'",e:"'",i:"\\n",c:[this.BE],r:0};this.QSM={cN:"string",b:'"',e:'"',i:"\\n",c:[this.BE],r:0};this.CLCM={cN:"comment",b:"//",e:"$"};this.CBLCLM={cN:"comment",b:"/\\*",e:"\\*/"};this.HCM={cN:"comment",b:"#",e:"$"};this.NM={cN:"number",b:this.NR,r:0};this.CNM={cN:"number",b:this.CNR,r:0};this.BNM={cN:"number",b:this.BNR,r:0};this.inherit=function(r,s){var p={};for(var q in r){p[q]=r[q]}if(s){for(var q in s){p[q]=s[q]}}return p}}();hljs.LANGUAGES.cpp=function(){var a={keyword:{"false":1,"int":1,"float":1,"while":1,"private":1,"char":1,"catch":1,"export":1,virtual:1,operator:2,sizeof:2,dynamic_cast:2,typedef:2,const_cast:2,"const":1,struct:1,"for":1,static_cast:2,union:1,namespace:1,unsigned:1,"long":1,"throw":1,"volatile":2,"static":1,"protected":1,bool:1,template:1,mutable:1,"if":1,"public":1,friend:2,"do":1,"return":1,"goto":1,auto:1,"void":2,"enum":1,"else":1,"break":1,"new":1,extern:1,using:1,"true":1,"class":1,asm:1,"case":1,typeid:1,"short":1,reinterpret_cast:2,"default":1,"double":1,register:1,explicit:1,signed:1,typename:1,"try":1,"this":1,"switch":1,"continue":1,wchar_t:1,inline:1,"delete":1,alignof:1,char16_t:1,char32_t:1,constexpr:1,decltype:1,noexcept:1,nullptr:1,static_assert:1,thread_local:1,restrict:1,_Bool:1,complex:1},built_in:{std:1,string:1,cin:1,cout:1,cerr:1,clog:1,stringstream:1,istringstream:1,ostringstream:1,auto_ptr:1,deque:1,list:1,queue:1,stack:1,vector:1,map:1,set:1,bitset:1,multiset:1,multimap:1,unordered_set:1,unordered_map:1,unordered_multiset:1,unordered_multimap:1,array:1,shared_ptr:1}};return{dM:{k:a,i:"</",c:[hljs.CLCM,hljs.CBLCLM,hljs.QSM,{cN:"string",b:"'\\\\?.",e:"'",i:"."},{cN:"number",b:"\\b(\\d+(\\.\\d*)?|\\.\\d+)(u|U|l|L|ul|UL|f|F)"},hljs.CNM,{cN:"preprocessor",b:"#",e:"$"},{cN:"stl_container",b:"\\b(deque|list|queue|stack|vector|map|set|bitset|multiset|multimap|unordered_map|unordered_set|unordered_multiset|unordered_multimap|array)\\s*<",e:">",k:a,r:10,c:["self"]}]}}}();hljs.LANGUAGES.r={dM:{c:[hljs.HCM,{cN:"number",b:"\\b0[xX][0-9a-fA-F]+[Li]?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+(?:[eE][+\\-]?\\d*)?L\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+\\.(?!\\d)(?:i\\b)?",e:hljs.IMMEDIATE_RE,r:1},{cN:"number",b:"\\b\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\.\\d+(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"keyword",b:"(?:tryCatch|library|setGeneric|setGroupGeneric)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\.",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\d+(?![\\w.])",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\b(?:function)",e:hljs.IMMEDIATE_RE,r:2},{cN:"keyword",b:"(?:if|in|break|next|repeat|else|for|return|switch|while|try|stop|warning|require|attach|detach|source|setMethod|setClass)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"literal",b:"(?:NA|NA_integer_|NA_real_|NA_character_|NA_complex_)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"literal",b:"(?:NULL|TRUE|FALSE|T|F|Inf|NaN)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"identifier",b:"[a-zA-Z.][a-zA-Z0-9._]*\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"<\\-(?!\\s*\\d)",e:hljs.IMMEDIATE_RE,r:2},{cN:"operator",b:"\\->|<\\-",e:hljs.IMMEDIATE_RE,r:1},{cN:"operator",b:"%%|~",e:hljs.IMMEDIATE_RE},{cN:"operator",b:">=|<=|==|!=|\\|\\||&&|=|\\+|\\-|\\*|/|\\^|>|<|!|&|\\||\\$|:",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"%",e:"%",i:"\\n",r:1},{cN:"identifier",b:"`",e:"`",r:0},{cN:"string",b:'"',e:'"',c:[hljs.BE],r:0},{cN:"string",b:"'",e:"'",c:[hljs.BE],r:0},{cN:"paren",b:"[[({\\])}]",e:hljs.IMMEDIATE_RE,r:0}]}};
hljs.initHighlightingOnLoad();
</script>

streamR: Access to Twitter Streaming API via R
---------

This package includes a series of functions that give R users access to Twitter&#39;s <a href="https://dev.twitter.com/docs/streaming-apis">Streaming API</a>, as well as a tool that parses the captured tweets and transforms them in R data frames, which can then be used in subsequent analyses. <code>streamR</code> requires authentication via OAuth and the <code>ROAuth</code> package.</p>

Current CRAN release is 0.2. To install most updated version (0.2) from GitHub, type:

```
library(devtools)
install_github("streamR", "pablobarbera", subdir="streamR")
```

Click <a href="http://github.com/pablobarbera/streamR/blob/master/streamR-manual.pdf?raw=true">here</a> to read the documentation and <a href="http://pablobarbera.com/blog/archives/1.html">here</a> to read the vignette.

<h3>Installation and authentication</h3>

<p>streamR can be installed directly from CRAN, but the most updated version <a href="https://github.com/pablobarbera/streamR">will always be on GitHub</a>. The code below shows how to install from both sources.</p>

<pre><code class="r">install.packages(&quot;streamR&quot;)  # from CRAN
library(devtools)
install_github(&quot;streamR&quot;, &quot;pablobarbera&quot;, subdir = &quot;streamR&quot;)  # from GitHub
</code></pre>


<p><code>streamR</code> requires authentication via OAuth. The same oauth token can be used for both <code>twitteR</code> and <code>streamR</code>. After 
creating an application <a href="https://dev.twitter.com/apps/new">here</a>, and obtaining the consumer key and consumer secret, it is easy to create your own oauth credentials using the <code>ROAuth</code> package, which can be saved in disk for future sessions:</p>

<pre><code class="r">library(ROAuth)
requestURL &lt;- &quot;https://api.twitter.com/oauth/request_token&quot;
accessURL &lt;- &quot;https://api.twitter.com/oauth/access_token&quot;
authURL &lt;- &quot;https://api.twitter.com/oauth/authorize&quot;
consumerKey &lt;- &quot;xxxxxyyyyyzzzzzz&quot;
consumerSecret &lt;- &quot;xxxxxxyyyyyzzzzzzz111111222222&quot;
my_oauth &lt;- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
    requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file(&quot;CurlSSL&quot;, &quot;cacert.pem&quot;, package = &quot;RCurl&quot;))
save(my_oauth, file = &quot;my_oauth.Rdata&quot;)
</code></pre>




<h3>filterStream</h3>

<p><code>filterStream</code> is probably the most useful function. It opens a connection to the <a href="https://dev.twitter.com/docs/api/1.1/post/statuses/filter">Streaming API</a> that will return all tweets that contain one or more of the keywords given in the <code>track</code> argument. We can use this function to, for instance, capture public statuses that mention Obama or Biden:</p>

<pre><code class="r">library(streamR)
</code></pre>

<pre><code>## Loading required package: RCurl
## Loading required package: bitops
## Loading required package: rjson
</code></pre>

<pre><code class="r">load(&quot;my_oauth.Rdata&quot;)
filterStream(&quot;tweets.json&quot;, track = c(&quot;Obama&quot;, &quot;Biden&quot;), timeout = 120, 
  oauth = my_oauth)
</code></pre>

<pre><code>## Loading required package: ROAuth
## Loading required package: digest
## Capturing tweets...
## Connection to Twitter stream was closed after 120 seconds with up to 350 tweets downloaded.
</code></pre>

<pre><code class="r">tweets.df &lt;- parseTweets(&quot;tweets.json&quot;, simplify = TRUE)
</code></pre>

<pre><code>## 350 tweets have been parsed.
</code></pre>

<p>Note that here I&#39;m connecting to the stream for just two minutes, but ideally I should have the connection continuously open, with some method to handle exceptions and reconnect when there&#39;s an error. I&#39;m also using OAuth authentication (see below), and storing the tweets in a data frame using the <code>parseTweets</code> function. As I expected, Obama is mentioned more often than Biden at the moment I created this post:</p>

<pre><code class="r">c( length(grep(&quot;obama&quot;, tweets.df$text, ignore.case = TRUE)),
   length(grep(&quot;biden&quot;, tweets.df$text, ignore.case = TRUE)) )
</code></pre>

<pre><code>## [1] 347  2
</code></pre>

<p>Tweets can also be filtered by two additional parameters: <code>follow</code>, which can be used to include tweets published by only a subset of Twitter users, and <code>locations</code>, which will return geo-located tweets sent within bounding boxes defined by a set of coordinates. Using these two options involves some additional complications &ndash; for example, the Twitter users need to be specified as a vector of user IDs and not just screen names, and the <code>locations</code> filter is incremental to any keyword in the <code>track</code> argument. For more information, I would suggest to check Twitter&#39;s <a href="http://dev.twitter.com/docs/streaming-apis/parameters">documentation</a> for each parameter.</p>

<p>Here&#39;s a quick example of how one would capture and visualize tweets sent from the United States:</p>

<pre><code class="r">filterStream(&quot;tweetsUS.json&quot;, locations = c(-125, 25, -66, 50), timeout = 300, 
    oauth = my_oauth)
tweets.df &lt;- parseTweets(&quot;tweetsUS.json&quot;, verbose = FALSE)
library(ggplot2)
library(grid)
map.data &lt;- map_data(&quot;state&quot;)
points &lt;- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))
points &lt;- points[points$y &gt; 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = &quot;white&quot;, 
    color = &quot;grey20&quot;, size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), &quot;lines&quot;)) + geom_point(data = points, 
    aes(x = x, y = y), size = 1, alpha = 1/5, color = &quot;darkblue&quot;)
</code></pre>

<center><img src="images/map.png" alt="Map of tweets" style="width: 750px;"/></center>

<h3>sampleStream</h3>

<p>The function <code>sampleStream</code> allows the user to capture a small random sample (around 1%) of all tweets that are being sent at each moment. This can be useful for different purposes, such as estimating variations in &ldquo;global sentiment&rdquo; or describing the average Twitter user. A quick analysis of the public statuses captured with this method shows, for example, that the average (active) Twitter user follows around 500 other accounts, that a very small proportion of tweets are geo-located, and that Spanish is the second most common language in which Twitter users set up their interface.</p>

<pre><code class="r">sampleStream(&quot;tweetsSample.json&quot;, timeout = 120, oauth = my_oauth, verbose = FALSE)
tweets.df &lt;- parseTweets(&quot;tweetsSample.json&quot;, verbose = FALSE)
mean(as.numeric(tweets.df$friends_count))
</code></pre>

<pre><code>## [1] 543.5
</code></pre>

<pre><code class="r">table(is.na(tweets.df$lat))
</code></pre>

<pre><code>## 
## FALSE  TRUE 
##   228 13503
</code></pre>

<pre><code class="r">round(sort(table(tweets.df$lang), decreasing = T)[1:5]/sum(table(tweets.df$lang)), 2)
</code></pre>

<pre><code>## 
##   en   es   ja   pt   ar 
## 0.57 0.16 0.09 0.07 0.03
</code></pre>

<h3>userStream</h3>

<p>Finally, I have also included the function <code>userStream</code>, which allows the user to capture the tweets they would see in their timeline on <a href="http://www.twitter.com">twitter.com</a>. As was the case with <code>filterStream</code>, this function allows to subset tweets by keyword and location, and exclude replies across users who are not followed. An example is shown below. Perhaps not surprisingly, many of the accounts I follow use Twitter in Spanish.</p>

<pre><code class="r">userStream(&quot;mytweets.json&quot;, timeout = 120, oauth = my_oauth, verbose = FALSE)
tweets.df &lt;- parseTweets(&quot;mytweets.json&quot;, verbose = FALSE)
round(sort(table(tweets.df$lang), decreasing = T)[1:3]/sum(table(tweets.df$lang)), 2)
</code></pre>

<pre><code>## 
##   en   es   ca 
## 0.62 0.30 0.08
</code></pre>

<h3>More</h3>

<p>In these examples I have used <code>parseTweets</code> to read the captured tweets from the text file where they were saved in disk and store them in a data frame in memory. The tweets can also be stored directly in memory by leaving the <code>file.name</code> argument empty, but my personal preference is to save the raw text, usually in different files, one for each hour or day. Having the files means I can run UNIX commands to quickly compute the number of tweets in each period, since each tweet is saved in a different line:</p>

<pre><code class="r">system(&quot;wc -l &#39;tweetsSample.json&#39;&quot;, intern = TRUE)
</code></pre>

<pre><code>## [1] &quot;   15086 tweetsSample.json&quot;
</code></pre>

<h3>Concluding...</h3>

<p>I hope this package is useful for R users who want to at least play around with this type of data. Future releases of the package will include additional functions to analyze captured tweets, and improve the already existing so that they handle errors better. My plan is to keep the <a href='https://github.com/pablobarbera/Rfacebook'>GitHub version</a> up to date fixing any possible bugs, and release only major versions to CRAN.</p>

<p>You can contact me at pablo.barbera[at]nyu.edu or via twitter (<a href="http://www.twitter.com/p_barbera">@p_barbera</a>) for any question or suggestion you might have, or to report any bugs in the code.</p>



