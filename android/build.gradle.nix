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
        classpath 'com.android.tools.build:gradle:2.3.0'
        ${googleServicesClasspath}
    }
}

allprojects {
    repositories {
        mavenLocal()
    }
}
apply plugin: 'com.android.application'

android {
    compileSdkVersion 25
    buildToolsVersion '26.0.1'

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
        targetSdkVersion 25
        versionCode ${version.code}
        versionName "${version.name}"
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

    buildTypes {
        ${if releaseKey == null then "" else ''
            release {
                minifyEnabled false
                zipAlignEnabled true
                signingConfig signingConfigs.release
            }
          ''
        }
        debug {
            minifyEnabled false
            debuggable true
        }
    }

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

import com.android.build.OutputFile

android.applicationVariants.all { variant ->
  variant.outputs.each { output ->
    def baseAbiVersionCode =
      project.ext.abiCodes.get(output.getFilter(OutputFile.ABI))

    if (baseAbiVersionCode != null) { // this will be null if splitting was disabled
      output.versionCodeOverride = baseAbiVersionCode * 1000 + variant.versionCode
    }
  }
}

dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])
    ${additionalDependencies}
}

${googleServicesPlugin}
''
