"use strict";

function currentSpeed() {
    return parseFloat($("#speed").val());
}

function applyPoints(dataById, points) {
    $.each(points, function(index, value) {
        dataById[value[2]] = value;
    });
}

function setupProcessingInstance(data) {
    var sketchProc = function (processing) {
        var assumedFrameRate = 60;
        var scaleFactor = 10;

        // Internal data structures--key is id, value is [x, y, id]
        var dataById = {};
        applyPoints(dataById, data.points);

        processing.setup = function() {
            processing.size(data.width*scaleFactor, data.length*scaleFactor);
        }

        var nextUpdateIdx = 0;
        var elapsedLogicalTime = 0;

        processing.draw = function() {
            var speed = currentSpeed();
            elapsedLogicalTime += speed/assumedFrameRate;
            var nextUpdate = data.updates[nextUpdateIdx];
            var nextTime = nextUpdate.time;
            if (elapsedLogicalTime > nextTime) {
                applyPoints(dataById, nextUpdate.points);
                if (nextUpdateIdx < data.updates.length-1)
                    ++nextUpdateIdx;
            }

            processing.background();
            processing.stroke(255,255,255);
            for(var x=0; x<data.width; ++x) {
                processing.line(scaleFactor*x, 0, scaleFactor*x, processing.height);
            }
            for(var y=0; y<data.length; ++y) {
                processing.line(0, scaleFactor*y, processing.width, scaleFactor*y);
            }
 
            processing.scale(scaleFactor);
            for(var id in dataById) {
                processing.fill(0,0,0);
                processing.noStroke();
                var point = dataById[id];
                processing.ellipse(point[0], point[1], 0.5, 0.5);
            }
            $("#points").text(JSON.stringify(dataById));
            $("#elapsed").text(elapsedLogicalTime.toFixed(5));
        }
    }

    return sketchProc;
}

function bigSampleInput(num_points, num_steps, prob_move, width, length) {

    // Copied from http://www.colingodsey.com/javascript-gaussian-random-number-generator/:
    var nrand = function() {
	var x1, x2, rad, y1;
	do {
		x1 = 2 * Math.random() - 1;
		x2 = 2 * Math.random() - 1;
		rad = x1 * x1 + x2 * x2;
	} while(rad >= 1 || rad == 0);
	var c = Math.sqrt(-2 * Math.log(rad) / rad);
	return x1 * c;
    };

    var sampleInput = {
        points: [],
        length: length,
        width: width,
        updates: []
    };
    
    var dataByIndex = [];
    for(var i=0; i<num_points; ++i) {
        var newPoint = [Math.random()*width, Math.random()*length, "id."+i.toString()];
        dataByIndex.push(newPoint);
        sampleInput.points.push(newPoint);
    }
    
    for(var j=0; i=j<num_steps; ++j) {
        var step_updates = [];
        for(var i=0; i<num_points; ++i) {
            if (Math.random() < prob_move) {
                var point = dataByIndex[i];
                step_updates.push([point[0]+nrand(), point[1]+nrand(), point[2]]);
            }
        }
        sampleInput.updates.push({time: j, points: step_updates});
    }

    return sampleInput;
}