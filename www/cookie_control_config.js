var config = {
    apiKey: '42ea541c155f97c6ee424bbdd7a609c658e87a4c',
    product: 'COMMUNITY',
    text : {
        thirdPartyTitle : 'Warning: Some cookies require your attention',
        thirdPartyDescription : 'Consent for some third party cookies can not be automatically revoked. Please follow the link below if you want to opt out of them.'
    },
    optionalCookies: [
        {
            name : 'analytics',
            label: 'Analytical Cookies',
            description: 'Analytical cookies help us to improve our website by collecting and reporting information on its usage.',
            cookies: ['_ga', '_gid', '_gat', '__utma', '__utmt', '__utmb', '__utmc', '__utmz', '__utmv'],
            onAccept : function(){
                // Add Google Analytics
                window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                gtag('config', 'G-S07KP4MCSN', { 'anonymize_ip':true });
                // End Google Analytics
            },
            onRevoke: function(){
                // Disable Google Analytics
                window['ga-disable-G-S07KP4MCSN'] = true;
                // End Google Analytics
            }
        }
    ],
    position: 'RIGHT',
    theme: 'LIGHT'
};

CookieControl.load( config );