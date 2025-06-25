## LLM playground

This is a web application that allows you to interact with an LLM using the [ellmer](https://ellmer.tidyverse.org/) R package. It provides a playground style interface for you to send prompts and receive responses from the model within a chat UI. 

It's intended to empower domain experts to write their own prompts and interact with the model without needing to write code. Users can select from the different providers and models available in the ellmer package, define optional API arguments, and customize their prompts to suit their needs.

Ensure that you have ellmer package setup and configured with your API keys before running the application. You can find more information on how to set up the ellmer package in the [ellmer documentation](https://ellmer.tidyverse.org/).

You can clone the repo via:

```
git clone https://github.com/parmsam/ellmerplayground.git
```
 
Or you can run it directly in RStudio by using the `runGithub()` function:

```
library(shiny)
runGitHub("ellmerplayground", "parmsam")
```
