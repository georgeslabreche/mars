// A scraper written in Javascript.
//
// It scrapes rover status data from the Opportunity rover's mission update side:
//  https://mars.nasa.gov/mer/mission/rover-status/opportunity
//
// Made to work only for entries from 2009 and onwards.
//
// Instructions:
//  1. Open a year page for the rover's mission update.
//     E.g.:https://mars.nasa.gov/mer/mission/rover-status/opportunity/2009/all/
//  2. Open the Web Console from your browser's Web Developer toolkit.
//  3. Copy and paste this code into the Console.
//  4. Hit 'Enter' to run the code.
//  5. A CSV string will be outputted containing all the extracted data.
//  6. Copy and paste that string into a file.
//  7. Save the file as a .csv file.
//  8. File can now be opened in a spreadsheet application or read and processed further with code.

// For date formatting.
var months = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"];

// Group indices of interest:
//  1 - Sol.
//  3 - Date.
//  4 - Generated energy (Wh).
//  5 - Tau factor,
//  6 - Solar array dust factor. Perfectly clean solar arrays would have a dust factor of 1.0, so the larger the dust factor, the cleaner the arrays.


// Array that will contain all of our data.
var data = []

// Loop through each paragraph element.
$("p").each(function(index) {

  // Extract desired data based on regex pattern matching.
  var regex = /As of Sol\s(\d{4})(\s\((.*)\)){0,1}.*\s(\d{1,4})\swatt-hours.*\(Tau\)\sof (\d{1}\.\d{1,3}).*dust factor of (\d{1}\.\d{1,4})\./gmi;
  var match = regex.exec($(this).text());

  // Extract data if text segment matches regex pattern.
  if(match !== null){

    // If we have a date, parse it.
    var date = '';
    if(match[3] != undefined){
      date = new Date(match[3].replace('.', ''));
      if(months[date.getMonth()] == undefined){
        // Date is invalid.
        date = ''
      }else{
        // Date is valid.
        date = date.getDate() + "-" + months[date.getMonth()] + "-" + date.getFullYear();
      }
    }

    var entry = {
      'Sol': parseInt(match[1]),
      'Date': date,
      'Wh': parseInt(match[4]),
      'TauFactor': parseFloat(match[5]),
      'SADustFactor': parseFloat(match[6])
    }

    // Store each entry object into array.
    data.push(entry);
  }
});

// Now that we have the data, we can process it into a CSV string.
// We do this using the very excellent Papa Parse library.
$.getScript("https://cdn.rawgit.com/mholt/PapaParse/master/papaparse.min.js", function() {
  // Convert data array into CSV string.
  csv_string = Papa.unparse(data, {newline: "\r",});

  // Print out result CSV string.
  console.log(csv_string);
});
