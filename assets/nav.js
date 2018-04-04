$(document).ready(function() {
    loginPage();

    $.ajaxPrefilter(function(opts, origOpts, xhr) {
        xhr.setRequestHeader('Authorization', 'Bearer ' + getCookie());
    });
});

function getCookie() {
    r = document.cookie.match(new RegExp('JWT-Cookie=([^;]+)'));
    if(r) return r[1];
}

var KEY = getCookie();

function loginPage() {
    var wrapper = $("<div>");
    var u = $('<input name="username" class="username"><br/>');
    var p = $('<input name="password" class="password" type="password"><br/>');
    var button = $('<input type="submit" value="Login">');

    button.click(function() {
        username = u[0].value;
        password = p[0].value;

        $.ajax('http://localhost:8081/login',{
            contentType: 'application/json',
            crossDomain: true,
            data: JSON.stringify({"username": username, "password": password}),
            method: 'POST',
            complete: function(x, s) {
                console.log(x);
            },
            failure: function(o,s,x) {
                console.log(x);
            }
        }); 
    });

    wrapper.append(u);
    wrapper.append(p);
    wrapper.append(button);
    $('#content').html(wrapper);
}

function allCourses() {

}