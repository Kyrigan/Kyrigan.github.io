/**
 * Created by ASUS on 5/20/2016.
 */

unirest.get("https://wordsapiv1.p.mashape.com/words/bump/also")
    .header("X-Mashape-Key", "sa9TqmLshAmshXb4OWkQTcqQH5D7p1JMVdgjsn0nhzdorVGN5S")
    .header("Accept", "application/json")
    .end(function (result) {
        console.log(result.status, result.headers, result.body);
    });