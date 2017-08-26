{ googleServicesClasspath
, googleServicesPlugin
, applicationId
, versionCode
, versionName
, additionalDependencies
# releaseKey should be null for a debug build, or something like this for a release build:
# { storeFile = ./android/keystore;
#   storePassword = "password";
#   keyAlias = "myKey";
#   keyPassword = "password";
# }
, releaseKey ? null
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
    buildToolsVersion '25.0.1'

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
        minSdkVersion 14
        targetSdkVersion 25
        versionCode ${versionCode}
        versionName "${versionName}"
    }

    ${if releaseKey == null then "" else ''
        signingConfigs {
          release {
            storeFile file(${releaseKey.storeFile})
            storePassword ${releaseKey.storePassword}
            keyAlias ${releaseKey.keyAlias}
            keyPassword ${releaseKey.keyPassword}
          }
        }
      ''
    }

    buildTypes {
        ${if releaseKey == null then "" else ''
            release {
                minifyEnabled false
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
}

dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])
    ${additionalDependencies}
}

${googleServicesPlugin}
''
