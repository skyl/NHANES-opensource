function pieChartFromCounts(selector, counts) {
  // http://nvd3.org/examples/pie.html
  console.log(counts);
  nv.addGraph(function() {
    var chart = nv.models.pieChart()
        .x(function(d) { return d.label })
        .y(function(d) { return d.value })
        .showLabels(true);
    d3.select(selector).append("svg")
        .datum(counts)
        .transition().duration(350)
        .call(chart);
    return chart;
  });
}

function histogramFromArray(selector, values) {
  // values is a flat array

  // use CSS to set width/height of histogram.
  var rect = d3.select(selector).node().getBoundingClientRect();
  var width = rect.width;
  var height = rect.height;

  var margin = {top: 10, right: 30, bottom: 30, left: 30};
  width = width - margin.left - margin.right,
  height = height - margin.top - margin.bottom;

  var x = d3.scale.linear()
      .domain([0, 85])
      .range([0, width]);

  // Generate a histogram using twenty uniformly-spaced bins.
  var data = d3.layout.histogram()
      .bins(x.ticks(40))
      (values);

  var y = d3.scale.linear()
      .domain([0, d3.max(data, function(d) { return d.y; })])
      .range([height, 0]);

  var xAxis = d3.svg.axis()
      .scale(x)
      .orient("bottom");

  var svg = d3.select(selector).append("svg")
      .attr("width", width)
      .attr("height", height)
    .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var bar = svg.selectAll(".bar")
      .data(data)
    .enter().append("g")
      .attr("class", "bar")
      .attr("transform", function(d) { return "translate(" + x(d.x) + "," + y(d.y) + ")"; });

  bar.append("rect")
      .attr("x", 1)
      .attr("width", x(data[0].dx) - 1)
      .attr("height", function(d) { return height - y(d.y); });

  // option to turn on text labels?
  /*
  bar.append("text")
      .attr("dy", ".75em")
      .attr("y", 6)
      .attr("x", x(data[0].dx) / 2)
      .attr("text-anchor", "middle")
      .text(function(d) { return d.y; });
  */

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);
}

function countCategory(data, field, translation) {
  //console.log(translation);
  // given data and a field eg ridreth1
  var key;
  var res = {};
  for (var i = data.length - 1; i >= 0; i--) {
    key = data[i][field];
    if (translation !== undefined && translation[key] !== undefined) {
      if (translation[key] === null) {
        // special case, throw away
        continue;
      }
      key = translation[key];
    }
    if (res[key] === undefined) {
      res[key] = 1;
    } else {
      res[key]++;
    }
  }
  //console.log(res);
  return objectToList(res);
}

function listOfValues(data, column) {
  // extract a column from the data
  var res = [];
  for (var i = data.length - 1; i >= 0; i--) {
    res.push(data[i][column]);
  }
  return res;
}

function objectToList(object) {
  res = [];
  for (var key in object) {
    if (object.hasOwnProperty(key)) {
      res.push({
        label: key,
        value: object[key]
      });
    }
  }
  return res;
}

function visualize(data) {
  // Gender
  var genderTranslation = {
    1: "Male",
    2: "Female"
  };
  pieChartFromCounts("#chartGenders", countCategory(data, "riagendr", genderTranslation));

  // Age
  histogramFromArray("#chartAges", listOfValues(data, "ridageyr"));

  // Race
  var raceTranslation = {
    1: "Mexican",
    2: "Other Hispanic",
    3: "White",
    4: "Black",
    5: "Other/Multi"
  };
  var raceCounts = countCategory(data, "ridreth1", raceTranslation);
  pieChartFromCounts("#chartRaces", raceCounts);

  // Military
  var militTranslation = {
    1: "Yes",
    2: "No",
    7: "Refused",
    9: "Don't know",
    "NA": "Missing"
  };
  var militCounts = countCategory(data, "dmqmilit", militTranslation);
  pieChartFromCounts("#chartMilitary", militCounts);

  // Country of birth
  var dmdBornTranslation = {
    1: "USA",
    2: "Mexico",
    3: "Other",
    7: "Refused",
    9: "Don't Know"
  };
  var dmdBornCounts = countCategory(data, "dmdborn", dmdBornTranslation);
  pieChartFromCounts("#chartBirthCountry", dmdBornCounts);

  // nvd3 tooltip does not escape `>`
  // so, this breaks the layout.
  // however &lt; doesn't work because the label is placed literally in svg text
  /*
  var dmdyrsusTranslation = {
    1: "t<1",
    2: "1<t<5",
    3: "5<t<10",
    4: "10<t<15",
    5: "15<t<20",
    6: "20<t<30",
    7: "30<t<40",
    8: "40<t<50",
    9: "50<t",
    77: null,
    88: null,
    99: null,
    "NA": null
  }
  var dmdYrsUSCounts = countCategory(data, "dmdyrsus", dmdyrsusTranslation);
  pieChartFromCounts("#chartYrsUS", dmdYrsUSCounts);
  */




}











Papa.parse("/1999-2000/csv/DEMO.csv", {
  download: true,
  header: true,
  dynamicTyping: true,
  // No streaming with SimpleHTTPServer
  // https://github.com/mholt/PapaParse/issues/94
  //step: function(row) {
	// 	console.log("Row:", row.data);
	//},	
  complete: function(result) {
    window.result = result;
    visualize(result.data);
  }
});



