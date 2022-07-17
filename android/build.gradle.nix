{ applicationId
, version
, releaseKey
, additionalDependencies
, googleServicesClasspath
, googleServicesPlugin
, universalApk
}:
''

buildscript {
    repositories {
        mavenLocal()
    }
   dependencies {
      classpath 'com.android.tools.build:gradle:7.2.1'
      classpath "com.android.tools.lint:lint-gradle:30.2.1"
      // implementation 'com.android.tools.lint:lint-gradle:30.2.1'
    }
    ext {
        ndkVersion = "22.0.7026061"
     }
 }

plugins {
    id 'com.android.application' version '7.2.1' apply false
}

allprojects {
    repositories {
        mavenLocal()
   }
}
apply plugin: 'com.android.application'
android {
    compileSdkVersion 31
    buildToolsVersion '31.0.0'

    sourceSets {
      main {
        manifest.srcFile 'AndroidManifest.xml'
        java.srcDirs = ['src']
        res.srcDirs = ['res']
        assets.srcDirs = ['assets'];
        jniLibs.srcDir 'lib'
      }
    }
    defaultConfig {
        applicationId "${applicationId}"
        minSdkVersion 21
        targetSdkVersion 31
        versionCode ${version.code}
        versionName "${version.name}"
        multiDexEnabled false
    }

    ${if releaseKey == null then "" else ''
        signingConfigs {
          release {
            storeFile file("${releaseKey.storeFile}")
            storePassword "${releaseKey.storePassword}"
            keyAlias "${releaseKey.keyAlias}"
            keyPassword "${releaseKey.keyPassword}"
          }
        }
      ''
    }

    // buildTypes {
    //     release {
    //         minifyEnabled false
    //         useProguard false
    //         zipAlignEnabled true
    //         ${if releaseKey == null then "" else ''
    //         signingConfig signingConfigs.release
    //         ''}
    //     }
    //     debug {
    //         minifyEnabled false
    //         useProguard false
    //         debuggable true
    //     }
    // }

    packagingOptions {
    }

    // see https://developer.android.com/studio/build/configure-apk-splits.html
    // for information about this and the applicationVariants stuff below.
    // See https://developer.android.com/google/play/publishing/multiple-apks.html#SingleAPK
    // for reasons you might not want to do this.
    ${if universalApk then "" else ''
        splits {
            abi {
                enable true
                reset()
                include "armeabi-v7a", "arm64-v8a"
                universalApk false
            }
        }
        ''
    }
}

ext.abiCodes = ['armeabi-v7a': 1, 'arm64-v8a': 2] // This order is important!

import com.android.build.api.variant.FilterConfiguration;
//import com.android.build.OutputFile.FilterType

android.applicationVariants.all { variant ->
  variant.outputs.each { output ->
    def baseAbiVersionCode = 1
    //  project.ext.abiCodes.get(output.filters.find( filter -> filter.filterType == FilterConfigurotion.FilterType.ABI).identifier);

    if (baseAbiVersionCode != null) { // this will be null if splitting was disabled
      output.versionCodeOverride = baseAbiVersionCode * 1000 + variant.versionCode
    }
  }
}

dependencies {
    implementation fileTree(dir: 'libs', include: ['*.jar'])
    implementation 'com.google.firebase:firebase-iid:21.1.0'
    implementation 'com.google.firebase:firebase-messaging:23.0.6'
    ${additionalDependencies}
}

${googleServicesPlugin}
''
