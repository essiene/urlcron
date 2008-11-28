{application, urlcron,
 [
    {description, "A cron implementation focused on the WEB and URLS"}, 
    {vsn, "1.0"}, 
    {modules, 
        [ 
            urlcron_scheduler, webservice,
            urlcron_mochiweb, urlcron_app,
            urlcron_sup, urlcron, urlcronctl
        ]
   },

  {registered, 
      [urlcron_sup, urlcron_mochiweb, urlcron_scheduler]
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

