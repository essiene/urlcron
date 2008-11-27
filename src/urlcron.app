{application, urlcron,
 [
    {description, "A cron implementation focused on the WEB and URLS"}, 
    {vsn, "1.0"}, 
    {modules, 
        [ 
            urlcron_main, urlcron_app, 
            urlcron_sup, urlcron_mochiweb
        ]
   },

  {registered, 
      [urlcron_sup, urlcron_mochiweb]
  },

  {applications, 
      [kernel, stdlib, crypto]
  },

  {mod, 
      {urlcron_app, []}
  },

  {start_phases, []}
 ]
}.

