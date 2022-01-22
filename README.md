## Get or Remove HTML element

Get or Remove HTML element(s) in a file by specifying a path    

The element path is of [Xpath](https://www.w3schools.com/xml/xpath_syntax.asp) syntax e.g:

```html/body/div[1]/div/div[2]```

means find in a given HTML or XML file, the second div tag element that is under every div, under the first div element, under any body element, under any html element.

```html/body/div[1]//div[1]/div[2]```

means find in a given HTML or XML file, the second div tag element that is under the first div element anywhere lives (by breadth or depth) under the first div element, under any body element, under any html element.

It may be put in multiply, delimited by ```;```
