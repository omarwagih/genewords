$( document ).ready(function() {
    $( ".tooltip-item" ).each(function(index) {
        tt = $( this );
        console.log( index + ": " + tt.text() );
        tt.wrap( '<span class="tooltip tooltip-effect-2"></span>'  );
        
        txt = ["<strong>Gene</strong>:\t" + tt.attr('gene') +"<br>",
               "<strong>Identifier</strong>:\t<a target=\"_blank\" href=\"http://www.uniprot.org/uniprot/"+tt.attr('acc')+"\">" + tt.attr('acc') + " (" + tt.attr('ent') + ")</a><br>", 
                      "<strong>Protein</strong>:\t" + tt.attr('prot') + "<br>",
                      "<strong>Organism</strong>:\t<i>" + tt.attr('org') +"</i>"];
        
        $( '<span class="tooltip-content clearfix"><span class="tooltip-text">\
                '+ txt.join('') +'\
                </span></span></span>' ).insertAfter( tt );
    });
    
    // Show hide certain navs
    if( $( "input" ).length > 0){
        $('#toggle_gene').hide();
        $('#try_another').hide();
    }else{
        $('#example').hide();
    }
    
    // Focus on input on load
    $('input').focus();
    
    // Toggle gene function
    $('#toggle_gene').click(function(){
        console.log('click')
        $('.tooltip-item').each(function() {
            tt = $(this)
            
            if(typeof tt.attr('gene_toggled') != "undefined"){
                console.log('not undefined')
                tt.text(tt.attr('word'));
                tt.removeAttr('gene_toggled')
            }else{
                console.log('undefined')
                tt.text(tt.attr('gene'));
                tt.attr('gene_toggled', '');
            }
        });
    });
    
    $('#example').click(function(){
         $("input").typed({
                strings: ["Genewords checks your words", "for matching gene names.", "Simply type in your sentence", "and hit enter", "Here's an example:", "This app is the best!"],
                typeSpeed: 5,
                callback: function (){
                    $('#sentence').val('This app is the best!');
                    setTimeout(function(){$('form').submit()}, 300);
                }
          });
    });
});
    