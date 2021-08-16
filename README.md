# PermissionManagerX [![Download](https://img.shields.io/github/v/release/mirfatif/PermissionManagerX?label="Download")](https://github.com/mirfatif/PermissionManagerX/releases/latest)
eXtended Permission Manager for Android - view and set Manifest Permissions and AppOps

<a href="https://f-droid.org/packages/com.mirfatif.permissionmanagerx"><img alt="Get it on F-Droid" src="https://fdroid.gitlab.io/artwork/badge/get-it-on.png" height="100"></a>
<a href="https://play.google.com/store/apps/details?id=com.mirfatif.permissionmanagerx"><img alt="Get it on Google Play" src="https://play.google.com/intl/en_us/badges/static/images/badges/en_badge_web_generic.png" height="100"></a>
<a href="https://apt.izzysoft.de/fdroid/index/apk/com.mirfatif.permissionmanagerx"><img alt="Get it on F-Droid" src="https://gitlab.com/IzzyOnDroid/repo/-/raw/master/assets/IzzyOnDroid.png" height="100"></a>
<a href="https://amzn.to/2Ij50j4"><img alt="Available at Amazon AppStore" src="https://user-images.githubusercontent.com/33040459/98554253-56c9d600-22c2-11eb-83ba-bd24fb1263ae.png" height="100"></a>

## Features
Using e<b>X</b>tended <b>Permission Manager</b>, for each installed app, on single screen, you can:

* View, grant or revoke manifest permissions
* View AppOps permissions and choose one of multiple modes
* Set your desired reference value for every changeable permission

The app evolved from a shell script to a GUI for my personal needs. After a ROM upgrade or changing device, it's a time-taking process to review all installed apps for granted permissions and revoke the unnecessary ones (after all  <b><i>privacy matters</i></b>). To come up with a solution, you can set <b>reference states</b> of permissions which can be quickly backed up and restored. Colored bars at left indicate reference states and make it quite easy to review packages and permissions at a glance.

<b>Manifest permissions</b> are those normally called permissions e.g. Storage, Camera etc. AppOps (app operations) is a robust framework Android uses at back end for access control. With every Android release manifest permissions are becoming more dependent on AppOps. So it's fun to control both simultaneously and see how they relate to each other.

In short, <b>AppOps</b> provide a fine-grained control over many of the manifest permissions. Plus it provides additional controls like background execution, vibration, clipboard access etc. Explore the app to see more. Also check <a href="https://mirfatif.github.io/PermissionManagerX/help/help.html#intro">What is PMX?</a>

## Required Privileges / Permissions

* In order to let Permission Manager X serve you at its best, either the device must be <b>rooted</b> or you need to enable <b>ADB over network</b>.
* <b>android.permission.INTERNET</b> is required to use ADB over network. The only connections made outside the device are to check for app updates or to fetch help contents.

## Note:

* The app is tested on stock Android 7-11. Some highly customized ROMs may behave unexpectedly.

## Privacy Policy

[https://mirfatif.github.io/PermissionManagerX/privacy_policy.html](https://mirfatif.github.io/PermissionManagerX/privacy_policy.html)

## Screenshots

<img src="fastlane/metadata/android/en-US/images/phoneScreenshots/1.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/2.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/3.jpg" width="250">
<img src="fastlane/metadata/android/en-US/images/phoneScreenshots/4.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/5.jpg" width="250"> <img src="fastlane/metadata/android/en-US/images/phoneScreenshots/6.jpg" width="250">

## How to Build
* `build.gradle` calls a shell script to build native binaries. So a Linux environment is expected with all standard tools.
* Download code: `git clone --depth=1 --recurse-submodules https://github.com/mirfatif/PermissionManagerX.git && cd PermissionManagerX`
* Set `sdk.dir` in `local.properties` to the directory containing Android SDK API level (platform) 30, build-tools 30.0.3 (and obviously the latest SDK `tools` and `platform-tools`), and Android NDK 22.1.
* Set `JAVA_HOME` environment variable to JDK 11 (`verifyGoogleJavaFormat` and some Annotations don't work with JDK 1.8). Or `java` and `javac` must be on `PATH`.
* Run `./gradlew` with appropriate task name appended. Or use IntelliJ IDEA / Android Studio.

## Translations [![Crowdin](https://badges.crowdin.net/pmx/localized.svg)](https://crowdin.com/project/pmx)
[Crowdin](https://crowdin.com/project/pmx)

## Third-Party Libraries
Credits and thanks to the developers of:
* [https://github.com/anggrayudi/android-hidden-api](https://github.com/anggrayudi/android-hidden-api)
* [https://github.com/cgutman/AdbLib](https://github.com/cgutman/AdbLib)
* [https://github.com/sherter/google-java-format-gradle-plugin](https://github.com/sherter/google-java-format-gradle-plugin)
* [https://github.com/material-components/material-components-android](https://github.com/material-components/material-components-android)
* [https://github.com/saket/Better-Link-Movement-Method](https://github.com/saket/Better-Link-Movement-Method)
* [https://github.com/square/leakcanary](https://github.com/square/leakcanary)

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

## Need help?

See guide and FAQs: [https://mirfatif.github.io/PermissionManagerX/help/help.html](https://mirfatif.github.io/PermissionManagerX/help/help.html)

## Contact Us

**Telegram:** [https://t.me/PermissionManagerX](https://t.me/PermissionManagerX)  
**Email:** [mirfatif@gmail.com](mailto:mirfatif@gmail.com)
