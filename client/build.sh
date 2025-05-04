rm -rf custom-runtime
rm -rf dist
mvn clean package

mvn dependency:copy-dependencies -DoutputDirectory=target/dependency

jlink --module-path "$JAVA_HOME/jmods:/home/kevin/Downloads/openjfx-21.0.6_linux-x64_bin-sdk/javafx-sdk-21.0.6/lib" \
      --add-modules java.se,jdk.zipfs,javafx.controls,javafx.fxml,javafx.graphics,javafx.web \
      --output custom-runtime

cp ~/Downloads/openjfx-21.0.6_linux-x64_bin-sdk/javafx-sdk-21.0.6/lib/* custom-runtime/lib/

jpackage --name DistributedStorageSystem --input target/ --main-jar distributed-storage-system-1.0-SNAPSHOT.jar --main-class com.unipi.application.Application --type app-image --dest dist/ --icon ./LogoDSS.png --runtime-image ./custom-runtime/

./dist/DistributedStorageSystem/bin/DistributedStorageSystem