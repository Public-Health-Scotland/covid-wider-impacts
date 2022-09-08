window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', 'G-YLEJLM420G', 
        { 'anonymize_ip': true ,
          'custom_map': {'dimension1': 'tab_viewed'}
        });

  ////////////////////////// Event tracking /////////////////////// 
  // Tracking clicks to the different tabs. It does track clicks to any links,
  // if no data value assigned they will appear as unset, but this could work to
  // look at all link click with some minor tweaks
     $(document).on('click', 'a', function(e) {
    gtag('event', 'tab_viewed', {'event_category' : 'TabsetPanel',
                                 'event_label' : $(this).attr('data-value'),
                                  'tab_name' : $(this).attr('data-value')
    });
    
  });
    