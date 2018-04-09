$(document).ready(function() {
    if(getUsername() != undefined && getPassword() != undefined) {
        home();
    } else {
        loginPage();
    }

    $('#login').click(loginPage);
    $('#allcourses').click(allCourses);
    $('#mycourses').click(myCourses);
    $('#myteachings').click(myTeachings);

});

function home() {
    allCourses();
    var logoutlink = $('<a id="logout" href="#">Log out</a>');
    logoutlink.click(logout);
    $('nav').append(logoutlink);
}

function getUsername() {
    r = document.cookie.match(new RegExp('username=([^;]+)'));
    if(r) return r[1];
}

function getPassword() {
    r = document.cookie.match(new RegExp('password=([^;]+)'));
    if(r) return r[1];
}

function loginPage() {
    var wrapper = $("<div>");
    var warning = $('<p style="color:red"></p>');
    var u = $('<input name="username" class="username"><br/>');
    var p = $('<input name="password" class="password" type="password"><br/>');
    var button = $('<input type="submit" value="Login">');

    button.click(function() {
        username = u[0].value;
        password = p[0].value;

        $.ajax('http://'+username+':'+password+'@localhost:8081/courses',{
            complete: function(x, s) {
                if(x.status == 403) {
                    // bad login
                    warning.html = "Wrong username or password!";
                    p.html = "";
                } else if(x.status == 200) {
                    // succesful login
                    warning.html = "";
                    document.cookie = "username="+username+";";
                    document.cookie = "password=" + password+";";
                    home();
                } else {
                    warning.html = "Error!"
                }
            }
        }); 
    });

    wrapper.append(warning);
    wrapper.append(u);
    wrapper.append(p);
    wrapper.append(button);
    $('#content').html(wrapper);
}

function logout() {
    $.removeCookie('username');
    $.removeCookie('password');
    $('#logout').remove();
    loginPage();
}

function allCourses() {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/courses', {
        complete: function(x, s) {
            if(x.status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                var c = $('#content');
                c.html('<h1>All courses</h1>');
                for(var i = 0; i < data.length; i++) {
                    var d = data[i];
                    c.append(drawCourseRough(d));
                }
            }
        }
    });
}

function myCourses() {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/mycourses', {
        complete: function(x, s) {
            if(x.status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                var c = $('#content');
                c.html('<h1>My courses</h1>');
                if(data.length == 0)
                    c.append('You do not follow any courses!');
                for(var i = 0; i < data.length; i++) {
                    var d = data[i];
                    c.append(drawCourseInfoRough(d));
                }
            }
        }
    });
}

function myTeachings() {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/myteachings', {
        complete: function(x, s) {
            if(x.status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                var c = $('#content');
                c.html('<h1>Courses you teach</h1>');
                if(data.length == 0)
                    c.append('You do not teach any courses!');
                for(var i = 0; i < data.length; i++) {
                    var d = data[i];
                    c.append(drawCourseInfoRough(d));
                }
            }
        }
    });
}

function getDetailedCourse(coursecode) {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/courses/'+coursecode, {
        complete: function(x, s) {
            if(x. status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                
                var c = $('#content');
                c.html(drawCourseDetailed(data));
            }
        }
    });
}

function getDetailedAssignment(id) {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/assignments/'+id, {
        complete: function(x, s) {
            if(x. status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                var c = $('#content');
                c.html(drawAssignmentDetailed(data));
            }
        }
    });
}

function getDetailedSubmission(assignmentid) {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/mysubmission/'+assignmentid, {
        complete: function(x, s) {
            if(x. status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                var c = $('#content');
                c.html(drawSubmissionDetailed(data, "student"));
            }
        }
    });
}

function getAllSubmisions(assignmentid) {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/allsubmissions/'+assignmentid, {
        complete: function(x, s) {
            if(x. status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                var c = $('#content');
                c.html('<h1>Submissions</h1>');
                for(var i = 0; i < data.length; i++) {
                    c.append(drawSubmissionRough(data[i]));
                }
            }
        }
    });
}

function getTeacherSubmission(submissionid) {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/submissions/'+submissionid, {
        complete: function(x, s) {
            if(x. status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                var c = $('#content');
                c.html(drawSubmissionDetailed(data, "teacher"));
            }
        }
    });
}

function drawCourseDetailed(data) {
    if(data == null) {
        return "<p>Course ID not found</p>";
    }
    var wrapper = $('<div>');
    var title=$('<h1>'+data.courseInfoName+'</h1>');
    wrapper.append(title);
    wrapper.append('<h4>'+data.courseInfoCourseCode+'</h4>');
    wrapper.append('<h3>Description</h3>');
    wrapper.append('<p>'+data.courseInfoDescription+'</p>');
    var teachers = $('<div>');
    teachers.append('<h3>Teachers</h3>');
    if(data.courseInfoTeachers.length == 0)
        teachers.append('<p>There are no teachers for this course. Anarchy!</p>');
    for(var i = 0; i < data.courseInfoTeachers.length; i++) {
        var d = data.courseInfoTeachers[i];
        teachers.append(drawTeacherInfo(d));
    }
    wrapper.append(teachers);
    var assignments = $('<div>');
    assignments.append('<h3>Assignments</h3>');
    if(data.assignments.length == 0)
        assignments.append('<p>There are no assignments!</p>');
    for(var i = 0; i < data.assignments.length; i++) {
        var d = data.assignments[i];
        assignments.append(drawAssignmentRough(d));
    }
    wrapper.append(assignments);
    return wrapper;
}

function drawAssignmentRough(data) {
    var wrapper = $('<div>');
    wrapper.append('<h4>'+data.name+'</h4>');
    wrapper.append('<p>Deadline: '+new Date(data.deadline)+'</p>');
    var link = $('<a href="#">Read more</a>')

    link.click(function() {
        getDetailedAssignment(data.id);
    });

    var p = $('<p>');
    p.append(link);
    wrapper.append(p);
    return wrapper;
}

function drawAssignmentDetailed(data) {
    if(data == null) {
        return '<p>Assignment ID not found</p>';
    }
    var wrapper = $('<div>');
    wrapper.append('<h1>'+data.assignmentInfoName+'</h1>');
    var subtitle = $('<h4>');
    var courselink = $('<a href="#">'+data.assignmentInfoCourse.coursename+'</a>');
    courselink.click(function() {
        getDetailedCourse(data.assignmentInfoCourse.coursecode);
    });
    subtitle.append(courselink);
    subtitle.append(' - '+new Date(data.assignmentInfoDeadline));
    subtitle.append(' - Maximum number of students: ' + data.assignmentInfoNrofstudents);
    if(data.assignmentInfoFollowing) {
        var submlink = $('<a href="#">My submission</a>');
        submlink.click(function() {
            getDetailedSubmission(data.assignmentInfoId);
        });
        var h3 = $('<h3>');
        h3.append(submlink);
        wrapper.append(h3);
    }
    if(data.assignmentInfoTeaching) {
        var submlink = $('<a href="#">View all submissions</a>');
        submlink.click(function() {
            getAllSubmisions(data.assignmentInfoId);
        });
        var h3 = $('<h3>');
        h3.append(submlink);
        wrapper.append(h3);
    }
    wrapper.append(subtitle);
    wrapper.append('<h3>Goal</h3>');

    wrapper.append('<p>'+data.assignmentInfoDescription+'</p>');

    return wrapper;
}

function drawTeacherInfo(data) {
    var table = '<table><tr><th>Name:</th><td>'+data.teacherInfoName+'</td></tr>' +
        '<tr><th>Office:</th><td>'+data.teacherInfoOffice+'</td></tr></table>';
    return $(table);
}

function drawCourseRough(data) {
    var wrapper = $('<div>');
    wrapper.append('<h2>'+data.coursename+'</h2>');
    wrapper.append('<h4>'+data.coursecode+'</h4>');
    wrapper.append('<h3>Description</h3>');
    wrapper.append('<p>'+data.description+'</p>');
    var link = $('<a href="#">Read more</a>');
    link.click(function() {
        getDetailedCourse(data.coursecode);
    });
    var p = $('<p>');
    p.append(link);
    wrapper.append(p);
    return wrapper;
}

function drawCourseInfoRough(data) {
    var newdata = {
        "coursename":data.courseInfoName,
        "coursecode":data.courseInfoCourseCode,
        "description":data.courseInfoDescription
    };
    return drawCourseRough(newdata);
}

function drawSubmissionDetailed(data, role) {
    var wrapper = $('<div>');
    var asslink = $('<a href="#">'+data.submissionInfoAssignment.name+'</a>');
    asslink.click(function() {
        getDetailedAssignment(data.submissionInfoAssignment.id);
    });
    var h1 = $('<h1>');
    h1.append('Submission for ');
    h1.append(asslink);
    wrapper.append(h1);
    var h3 = $('<h3>By: </h3>');
    for(var i = 0; i < data.submissionInfoStudents.length; i++) {
        var student = data.submissionInfoStudents[i];
        h3.append(student.studentInfoName);
        if(i < data.submissionInfoStudents.length - 1)
            h3.append(', ');
    }
    wrapper.append(h3);
    wrapper.append('<h2>Read me</h2>');
    wrapper.append('<p>'+data.submissionInfoReadme+'</p>');
    wrapper.append('<h2>Files</h2>');
    var table=$('<table id="files">');
    table.append('<tr><th>File name</th><th>File size</th><th>Binary</th></tr>');
    for(var i = 0; i < data.submissionInfoFiles.length; i++) {
        var file = data.submissionInfoFiles[i];
        drawFile(file,table);
    }
    
    wrapper.append(table);
    if(role == "student") {
        wrapper.append(drawUploadForm(data.submissionInfoId, table));
        wrapper.append(drawGrade(data));
    }
    if(role == "teacher") {
        wrapper.append(drawGrading(data));
    }
    return wrapper;
}

function drawGrade(data) {
    var wrapper = $('<div>');
    if(data.submissionInfoGradedby && data.submissionInfoGrade) {
        wrapper.append('<p>Grade: '+data.submissionInfoGrade+'</p>');
        wrapper.append('<p>Graded by: '+data.submissionInfoGradedby.name+'</p>');
        return wrapper;
    }
    return "";
}

function drawGrading(data) {
    var wrapper = $('<div>');
    var status = $('<p>');
    wrapper.append(status);
    wrapper.append('Grade: ');
    var input = $('<input placeholder="Grade" value="'+data.submissionInfoGrade+'">');
    wrapper.append(input);
    var button = $('<input type="submit" value="Update grade">')
    button.click(function() {
        updateGrade(data.submissionInfoId, input.val(), status);
    })
    wrapper.append(button);
    return wrapper;
}

function updateGrade(submissionid, grade, status) {
    var u = getUsername();
    var p = getPassword();
    $.ajax('http://'+u+':'+p+'@localhost:8081/grade/'+submissionid+'/'+grade, {
        complete: function(x, s) {
            if(x. status == 403) {
                logout();
            } else if(x.status == 200) {
                var data = x.responseJSON;
                if(data) {
                    status.text("Grade updated!");
                }
                else {
                    status.text("Something went wrong! Are you actually the course's teacher?");
                }
            } else {
                status.text("Something went wrong! Did you input a valid number?");
            }
        }
    });
}

function drawFile(file, table) {
    var link = '<a href="'+file.filepath.substr(7)+'">'+file.filename+'</a>';
    table.append('<tr><td>'+link+'</td><td style="text-align: right">'+file.filesize+'</td></tr>');
}

function drawUploadForm(submissionid, outputTable) {
    var u = getUsername();
    var p = getPassword();
    var form = $('<form method="post" accept-charset="utf-8" enctype="multipart/form-data" action="http://'
                  +u+':'+p+'@localhost:8081/upload/'+submissionid+'">');
    var file = $('<input type="file" name="file">');
    var button = $('<input type="submit" value="Upload">');

    button.click(function(e) {
        e.preventDefault();
        //Send file
        var fd = new FormData(form[0]);
        $.ajax(form.attr('action'), {
            contentType: false,
            processData: false,
            cache: false,
            enctype: "multipart/form-data",
            type: "POST",
            data: fd,
            complete: function(x, s) {
                if(x.status == 200) {
                    drawFile(x.responseJSON, outputTable);
                }
                else {
                    outputTable.after('<p style="color:red">Something went wrong! Are you authorized to do this?</p>');
                }
            }
        });
    });

    form.append(file);
    form.append(button);
    return form;
}

function drawSubmissionRough(data) {
    var wrapper = $('<div>');
    var h3 = $('<h3>Submission by </h3>');
    for(var i = 0; i < data.submissionInfoStudents.length; i++) {
        h3.append(data.submissionInfoStudents[i].studentInfoName);
        if(i < data.submissionInfoStudents.length - 1) {
            h3.append(', ');
        }
    }
    wrapper.append(h3);
    var grade = data.submissionInfoGrade;
    grade = grade ? grade : '?';
    //if(data.submissionInfoGradedby)
    var gradedby = data.submissionInfoGradedby;
    gradedby = gradedby ? gradedby.name : '?';

    wrapper.append('<p>Grade: '+grade+'</p>');
    wrapper.append('<p>Graded by: '+gradedby+'</p>');

    var link = $('<a href="#">Read more</a>');

    link.click(function() {
        getTeacherSubmission(data.submissionInfoId);
    })

    wrapper.append(link);
    return wrapper;
}