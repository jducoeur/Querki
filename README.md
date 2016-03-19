## The Querki Project

You know how everybody is all excited about Big Data these days? Huge, complex systems to process
billions of records in an enterprise environment? Yeah, this isn't that.

The Querki Project can best be thought of as the beginning of the Small Data revolution. It's a very
new and different sort of database/website, designed specifically for tiny, easy data-centric problems -- 
the sort of problems that ordinary people actually have day-to-day. This is anything from shopping lists 
to personal inventories to club organization to cookbooks: problems that are typically messy, poorly 
structured, fluid, evolving, involving small amounts of data, but nonetheless *do* have relevant structure.

These tend to get ignored by the engineering community, because each problem seems so *simple* -- they're 
the sort of things that a competent programmer could toss off in Rails in less than a day. But why would 
I want to spend a day on a problem like that? I should be able to set it up in *minutes*.

Or to put it another way: Querki is to Rails essentially as a wiki is to a static website. It's not as 
powerful as Rails, but it's a *heck* of a lot easier and quicker to use for the sorts of problems it can 
deal with. It focuses on ease of use for everyday users, rather than power for the programmers.

(Which isn't to say that it doesn't have power -- Querki's object-oriented data model makes a lot of messy 
problems much easier than a traditional RDBMS. But it is currently aimed at data sets of hundreds or 
thousands of records, not millions.)

For more information, see the Querki Documentation Space: http://www.querki.net/help/ -- in particular, 
the [Querki Quickstart](http://www.querki.net/help/#Querki-Quickstart) or 
[Learning Querki](http://www.querki.net/help/#Learning-Querki) -- or contact me directly at justin@querki.net

### License

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Querki</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/jducoeur/Querki" property="cc:attributionName" rel="cc:attributionURL">Querki Inc</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

Note that, while Querki itself is somewhat restricted, we are gradually lifting a few libraries out of it,
and releasing those under the MIT license for general use. See [the list of Querki Open-Source Projects](https://www.querki.net/help/#Querki-Open-Source-Projects) in the Querki documentation.
