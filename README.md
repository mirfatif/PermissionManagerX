# PermissionManagerX
eXtended Permission Manager for Android - view and set Manifest Permissions and AppOps

## Features
For each installed app, on single screen, you can:

* View, grant or revoke manifest permissions
* View AppOps permissions and choose one of multiple modes
* Set your desired reference value for every changeable permission

The app evolved from a shell script to a GUI for my personal needs. After a ROM upgrade or changing device, it's a time-taking process to review all installed apps for granted permissions and revoke the unnecessary ones (after all  <b><i>privacy matters</i></b>). To come up with a solution, you can set <b>reference states</b> of permissions which can be quickly backed up and restored. Colored bars at left indicate reference states and make it quite easy to review packages and permissions at a glance.

<b>Manifest permissions</b> are those normally called permissions e.g. Storage, Camera etc. AppOps (app operations) is a robust framework Android uses at back end for access control. With every Android release manifest permissions are becoming more dependent on AppOps. So it's fun to control both simultaneously and see how they relate to each other.

In short, <b>AppOps</b> provide a fine-grained control over many of the manifest permissions. Plus it provides additional controls like background execution, vibration, clipboard access etc. Explore the app to see more.

## Required Privileges / Permissions
In order to let Permission Manager X serve you at its best, either the device must be <b>rooted</b> or you need to enable <b>ADB over network</b>.
<b>android.permission.INTERNET</b> is required to use ADB over network. No connections are made outside the device.

## Screenshots

<img src="Screenshots/PMX1.jpg" width="250"> <img src="Screenshots/PMX2.jpg" width="250"> <img src="Screenshots/PMX3.jpg" width="250">
<img src="Screenshots/PMX4.jpg" width="250"> <img src="Screenshots/PMX5.jpg" width="250"> <img src="Screenshots/PMX6.jpg" width="250">
