function onOpen() {
var sheet = SpreadsheetApp.getActiveSpreadsheet();
var entries = [{
name : "Export Events",
functionName : "exportEvents"
}];     sheet.addMenu("Calendar Actions", entries);
};

function parseDate(s) {
var months = {jan:0,feb:1,mar:2,apr:3,may:4,jun:5,
            jul:6,aug:7,sep:8,oct:9,nov:10,dec:11};
var p = s.replace(".", "").split('-');
return new Date(p[2], months[p[1].toLowerCase()], p[0]);
}

/**
* Export events from spreadsheet to calendar
*/
function exportEvents() {
var sheet = SpreadsheetApp.getActiveSheet();

var headerRows = 6;  // Number of rows of header info (to skip)
var range = sheet.getDataRange();
var data = range.getDisplayValues();
//var calId = "Your calendar Id"; // PRODUCTION
var calId = "Your_calendar Id to test"; // TEST
var cal = CalendarApp.getCalendarById(calId);
//Logger.log(cal);
//Logger.log(data.length);
for (i=0; i<data.length; i++) {
if (i < headerRows) continue; // Skip header row(s)
if (data[i][0].length < 1) continue; // Skip if no content.

var row    = data[i];
Logger.log(row);
var date   = parseDate(row[0]);  // First column
//Logger.log(date);
var title  = row[1];s// Second column

var tstart = new Date();
var s = row[2].split(":");
tstart.setHours(s[0]);
tstart.setMinutes(s[1]);
tstart.setSeconds(s[2]);
tstart.setDate(date.getDate());
tstart.setMonth(date.getMonth());
tstart.setYear(date.getYear());

var tstop = new Date();
var e = row[3].split(":");
tstop.setHours(e[0]);
tstop.setMinutes(e[1]);
tstop.setSeconds(e[2]);

tstop.setDate(date.getDate());
tstop.setMonth(date.getMonth());
tstop.setYear(date.getYear());
var loc  = row[4];
var desc = row[5];
var id   = row[6];              // Sixth column == eventId
// Check if event already exists, update it if it does
var event = null;
if (id.length > 0) {
try {
event = cal.getEventSeriesById(id);
}
catch (e) {
// do nothing - we just want to avoid the exception when event doesn't            exist
} 
}
if (!event) {
//cal.createEvent(title, new Date("March 3, 2010 08:00:00"), new
//Date("March 3, 2010 09:00:00"), {description:desc,location:loc};
var newEvent = cal.createEvent(title, tstart, tstop,                                     
{description:desc,location:loc}).getId();                     
var r = i + 1;
var cell = sheet.getRange("G" + r);
cell.setValue(newEvent);
}
else {
Logger.log(event);
event.setTitle(title);
event.setDescription(desc);
event.setLocation(loc);
// event.setTime(tstart, tstop); // cannot setTime on eventSeries.
// ... but we CAN set recurrence!
var recurrence = CalendarApp.newRecurrence().addDailyRule().times(1);
event.setRecurrence(recurrence, tstart, tstop);
}
debugger;
}
}