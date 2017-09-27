{ applicationId
, version
, releaseKey
, additionalDependencies
, googleServicesClasspath
, googleServicesPlugin
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
