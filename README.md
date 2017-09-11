# pdf-scraping-R

Small project to extract the majors from university commencement programs stored in PDF format, using R. PDFs are notoriously difficult to scrape as there is often little structure to how the information is displayed on the page.

This program extracts the data from Bowdoin College, first converting the PDF to raw text, then employing a variety of techniques to parse the text using regular expressions and statistical measures. This program could be extended to similarly formatted PDFs, although each new PDF would likely need to receive individual treatment.
