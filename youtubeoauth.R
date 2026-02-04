# YouTube oAuth credentials 
# for security purposes this is not stored in the main code, 
# you need to fetch package tuber before you can authenticate! 
# optain your YouTube API credentials here: https://developers.google.com/youtube/registering_an_application
app_id          <- "<a very long number>.apps.googleusercontent.com"
app_secret      <- "<character>-<more characters>-<and some more>"
yt_oauth(app_id, app_secret, token = '')
