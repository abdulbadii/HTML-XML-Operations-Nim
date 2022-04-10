## Get, Remove or Copy Markup Language elements   

Do such operations in a file by specifying the path    

This should be of abbreviated [Xpath](https://www.w3.org/TR/1999/REC-xpath-19991116/#path-abbrev) syntax, e.g:    

```html/body/div[1]/div/div[2]```     

means find in a given HTML or XML file, the second div tag element node under every div element lies under the first div element, under any body element, under any html element.    

```html/body/div[3]//div[1]/div```    

means find in a given HTML or XML file, any div tag element node under the first div element lies at any depth under the third div element of any body element, under any html element.    

It may be put in multiply one after another delimited by ```;``` or ```|```    
For copying, append at the end the characters ```+>``` followed by the copy target path    
e.g.    

```html/body/div[1]/div/div[2]+>html/body/div[3]//div[1]/div```   
