# PermissionManagerX
eXtended Permission Manager for Android - view and set Manifest Permissions and AppOps

<a href="https://f-droid.org/packages/com.mirfatif.permissionmanagerx"><img alt="Get it on F-Droid" src="https://fdroid.gitlab.io/artwork/badge/get-it-on.png" height="100"></a>
<a href="https://play.google.com/store/apps/details?id=com.mirfatif.permissionmanagerx"><img alt="Get it on Google Play" src="https://play.google.com/intl/en_us/badges/static/images/badges/en_badge_web_generic.png" height="100"></a>  
<a href="https://apt.izzysoft.de/fdroid/index/apk/com.mirfatif.permissionmanagerx"><img alt="Get it on IzzyOnDroid" src="https://gitlab.com/IzzyOnDroid/repo/-/raw/master/assets/IzzyOnDroid.png" height="100"></a>
<a href="https://amzn.to/2Ij50j4"><img alt="Available at Amazon AppStore" src="https://user-images.githubusercontent.com/33040459/98554253-56c9d600-22c2-11eb-83ba-bd24fb1263ae.png" height="100"></a>  
<a href="https://mirfatif.github.io/mirfatif/getpro"><img alt="Get Pro Features" src="https://user-images.githubusercontent.com/33040459/137955720-51f667ab-ea09-4b1d-9626-0b857e5f1673.png" height="100"></a>

[![Github](https://img.shields.io/github/v/release/mirfatif/PermissionManagerX?label="Github")](https://github.com/mirfatif/PermissionManagerX/releases/latest) [![F-Droid](https://img.shields.io/f-droid/v/com.mirfatif.permissionmanagerx.svg?label="F-Droid")](https://f-droid.org/packages/com.mirfatif.permissionmanagerx) [![IzzyOnDroid](https://img.shields.io/endpoint?url=https://apt.izzysoft.de/fdroid/api/v1/shield/com.mirfatif.permissionmanagerx)](https://apt.izzysoft.de/fdroid/index/apk/com.mirfatif.permissionmanagerx) [![Telegram](https://img.shields.io/endpoint?url=https://mirfatif.github.io/mirfatif/pmx_version.json)](https://t.me/mirfatifApps)

<sup>* Join Telegram support group to get instant updates and test beta releases.</sup>

## Features
Using e<b>X</b>tended <b>Permission Manager</b>, for each installed app, on single screen, you can:

* View, grant or revoke manifest permissions
* View AppOps permissions and choose one of multiple modes
* Set your desired reference value for every changeable permission

<b>Manifest permissions</b> are those normally called permissions e.g. Storage, Camera etc. <b>AppOps</b> (app operations) is a robust framework Android uses at back end for access control. With every Android release manifest permissions are becoming more dependent on AppOps. So it's fun to control both simultaneously and see how they relate to each other.

In short, AppOps provide a fine-grained control over many of the manifest permissions. Plus it provides additional controls like background execution, vibration, clipboard access etc. Just install the app to explore it yourself.

When you reinstall an app, or change your device, or upgrade your ROM, it's a time-taking process to review all installed apps for granted permissions and revoke the unnecessary ones (after all  <b><i>privacy matters</i></b>). PMX provides you the solution. Set <b>reference states</b> of permissions, which can be quickly backed up and restored, and colored bars at left make it quite easy to review packages and permissions at a glance.

Confused? We are here to explain. Please start with:

* <a href="https://mirfatif.github.io/PermissionManagerX/help/en#intro">What is PMX?</a>
* <a href="https://mirfatif.github.io/PermissionManagerX/help/en#faq36">Why do I need to use PMX?</a>
* <a href="https://mirfatif.github.io/PermissionManagerX/help/en#perms_types">What are manifest permissions and AppOps?</a>
* <a href="https://mirfatif.github.io/PermissionManagerX/help/en#perm_ref">Permission References</a>

## Required Privileges / Permissions

* In order to let Permission Manager X serve you at its best, either the device must be <b>rooted</b> or you need to enable <b>ADB over network</b>.
* <b>android.permission.INTERNET</b> is required to use ADB over network. The only connections made outside the device are to check for app updates or to fetch help contents. Pro version also requires internet connection for license verification.

## Note:

* The app is tested on stock Android 7-15 (pro version). Some highly customized ROMs may behave unexpectedly.

## Privacy Policy

[Privacy Policy](https://mirfatif.github.io/PermissionManagerX/privacy_policy.html)

## Paid Features

[Paid Features](https://mirfatif.github.io/PermissionManagerX/help/en#paid_features)

## Screenshots

<img src="fastlane/metadata/android/en-US/images/phoneScreenshots/1.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/2.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/3.jpg" width="250">
<img src="fastlane/metadata/android/en-US/images/phoneScreenshots/4.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/5.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/6.jpg" width="250">

## How to Build
`build.gradle` calls a shell script to build native binaries. So a Linux environment is expected with all standard tools.
* Download code: `git clone --depth=1 --recurse-submodules https://github.com/mirfatif/PermissionManagerX.git && cd PermissionManagerX`
* Set `sdk.dir` in `local.properties` to the directory containing Android SDK
* Run `./gradlew :app:assembleRelease`. Or use IntelliJ IDEA / Android Studio.

## Translations [![Crowdin](https://badges.crowdin.net/pmx/localized.svg)](https://crowdin.com/project/pmx)
[Crowdin](https://crowdin.com/project/pmx)

## Third-Party Libraries
Credits and thanks to the developers of:
* [Android Jetpack](https://github.com/androidx/androidx)
* [Android Hidden APIs](https://github.com/anggrayudi/android-hidden-api)
* [LSPass](https://github.com/LSPosed/AndroidHiddenApiBypass)
* [LibADB Android](https://github.com/MuntashirAkon/libadb-android)
* [Spotless GoogleJavaFormat](https://github.com/diffplug/spotless)
* [Material Components for Android](https://github.com/material-components/material-components-android)
* [Guava](https://github.com/google/guava)
* [BetterLinkMovementMethod](https://github.com/saket/Better-Link-Movement-Method)
* [LeakCanary](https://github.com/square/leakcanary)

## License [![License](https://img.shields.io/github/license/mirfatif/PermissionManagerX?label="License")](https://github.com/mirfatif/PermissionManagerX/blob/master/LICENSE)

You **CANNOT** use and distribute the app icon in anyway, except for **Permission Manager X** (`com.mirfatif.permissionmanagerx`) app.

    Permission Manager X is free software: you can redistribute it and/or modify
    it under the terms of the Affero GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    Affero GNU General Public License for more details.

    You should have received a copy of the Affero GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Need Help?

See [Guide and FAQs](https://mirfatif.github.io/PermissionManagerX/help/help.html).

## Want To Support Us?

<table>
    <td><a href="https://www.buymeacoffee.com/mirfatif"><img src="https://cdn.buymeacoffee.com/buttons/default-white.png" height="45" width="170" alt="Buy Me A Coffee"/></a></td>
    <td>&nbsp;<a href="http://mirfatif.github.io/mirfatif/bitcoin_redirect"><img src="http://mirfatif.github.io/mirfatif/logos/btc_button.png" height="37" width="150" alt="Donate Bitcoin"/></a>&nbsp;</td>
</table>

&nbsp;&nbsp;&nbsp;(`bitcoin:18ijfsv5fcDKQ6CTe4wycKxZMmti4oUXjW`)

## Want To Reach Us?

<table>
    <td>&nbsp;<a href="https://t.me/PermissionManagerX"><img src="https://raw.githubusercontent.com/mirfatif/mirfatif/main/logos/telegram.png" height="25"/> Telegram</a>&nbsp;</td>
    <td><a href="https://forum.xda-developers.com/t/app-7-0-permission-manager-x-manage-appops-and-manifest-permissions.4187657"><img src="https://raw.githubusercontent.com/mirfatif/mirfatif/main/logos/xda.png" height="23" width="21"/> XDA Thread</a></td>
    <td>&nbsp;&nbsp;&nbsp;&nbsp;<a href="mailto:mirfatif.dev@gmail.com"><img src="https://raw.githubusercontent.com/mirfatif/mirfatif/main/logos/email.png" height="22" width="25"/> Email</a>&nbsp;&nbsp;&nbsp;&nbsp;</td>
</table>
