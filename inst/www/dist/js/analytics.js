

<script type="text/javascript">//<![CDATA[ 
window.onload=function(){
//set CORS to call "appdemo" package on public server
ocpu.seturl("http://public.opencpu.org/ocpu/github/JulianHill/SentimentClassR/R")

//global var
var mysession;

//calls R function: stats::rnorm(n=100, mean=runif(1)):
$("#postbutton").click(function(){

 ocpu.seturl("http://public.opencpu.org/ocpu/github/JulianHill/SentimentClassR/R")

//arguments
    var myheader = $("#header").val() == "true";
    


    //get the number
    fieldName = $('.btn-number').attr('data-field');
    var input = $("input[name='"+fieldName+"']");
    var currentVal = parseInt(input.val());



var name = $("#username").val();
var no = currentVal; 

window.confirm(name);
window.confirm(no);


     //perform the request
    var req = ocpu.call("read.csv", {
        "file" : myfile,
        "header" : myheader
    });

    
 

   $("#output").attr('src', '')
        $("#loading").show()
        var req = ocpu.call("Main", {
        file: myfile,  
        api_key: key,
        token_diff: diff,
        token_shares: shares,
        
        
     }, function(session){
       $("#loading").hide()
       $("#output").attr('src', session.getLoc() + "output.html");  
       $("#download").show()
       $scope.$apply(function(){
         $scope.dnlink = session.getLoc() + "files/output.html"
       })
     }).fail(function(text){
      alert("Error: " + req.responseText);
     });


});

$("#getbutton").click(function(){
    //retrieve object (async)
    mysession.getObject(function(data){
        //data is the object returned by the R function
        alert("Array of length " + data.length + ".\nFirst few values:" + data.slice(0,3)); 
    });
});

       

}//]]>  

</script>










    
    <script type="text/javascript">//<![CDATA[ 
    //plugin bootstrap minus and plus
//http://jsfiddle.net/laelitenetwork/puJ6G/
$('.btn-number').click(function(e){
    e.preventDefault();
    
    fieldName = $(this).attr('data-field');
    type      = $(this).attr('data-type');
    var input = $("input[name='"+fieldName+"']");
    var currentVal = parseInt(input.val());
    if (!isNaN(currentVal)) {
        if(type == 'minus') {
            
            if(currentVal > input.attr('min')) {
                input.val(currentVal - 1).change();
            } 
            if(parseInt(input.val()) == input.attr('min')) {
                $(this).attr('disabled', true);
            }

        } else if(type == 'plus') {

            if(currentVal < input.attr('max')) {
                input.val(currentVal + 1).change();
            }
            if(parseInt(input.val()) == input.attr('max')) {
                $(this).attr('disabled', true);
            }

        }
    } else {
        input.val(0);
    }
});
$('.input-number').focusin(function(){
   $(this).data('oldValue', $(this).val());
});
$('.input-number').change(function() {
    
    minValue =  parseInt($(this).attr('min'));
    maxValue =  parseInt($(this).attr('max'));
    valueCurrent = parseInt($(this).val());
    
    name = $(this).attr('name');
    if(valueCurrent >= minValue) {
        $(".btn-number[data-type='minus'][data-field='"+name+"']").removeAttr('disabled')
    } else {
        alert('Sorry, the minimum value was reached');
        $(this).val($(this).data('oldValue'));
    }
    if(valueCurrent <= maxValue) {
        $(".btn-number[data-type='plus'][data-field='"+name+"']").removeAttr('disabled')
    } else {
        alert('Sorry, the maximum value was reached');
        $(this).val($(this).data('oldValue'));
    }
    
    
});
$(".input-number").keydown(function (e) {
        // Allow: backspace, delete, tab, escape, enter and .
        if ($.inArray(e.keyCode, [46, 8, 9, 27, 13, 190]) !== -1 ||
             // Allow: Ctrl+A
            (e.keyCode == 65 && e.ctrlKey === true) || 
             // Allow: home, end, left, right
            (e.keyCode >= 35 && e.keyCode <= 39)) {
                 // let it happen, don't do anything
                 return;
        }
        // Ensure that it is a number and stop the keypress
        if ((e.shiftKey || (e.keyCode < 48 || e.keyCode > 57)) && (e.keyCode < 96 || e.keyCode > 105)) {
            e.preventDefault();
        }
    });
    
    </script>

