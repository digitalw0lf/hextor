# Get version tag (0.1-alpha)
ver_tag=$(git -C .. tag --sort=creatordate | tail -n1 | tr -d '\r\n')
# Version to program
rm ../Source/AppVersion.inc
(echo -n -e "const\r\n  AppVersion = '"; echo -n $ver_tag; echo -n -e "';\r\n") >> ../Source/AppVersion.inc
# Version to installer
rm ../Installer/AppVersionDefine.inc
(echo -n -e '#define MyAppVersion "'; echo -n $ver_tag; echo -n -e '"\r\n') >> ../Installer/AppVersionDefine.inc
# Version to web update info
sed -i -E "s/(<META NAME=\"mxVersion\" CONTENT=\")[^\"]*(\">)/\1$ver_tag\2/" ../../Hextor-pages/download/webupdate.htm 
