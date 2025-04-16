rm -rf custom-runtime

mvn clean package

mvn dependency:copy-dependencies -DoutputDirectory=target/dependency

jlink --module-path "$JAVA_HOME/jmods:/Users/aliceorlandini/IdeaProjects/Distributed-Storage-System/client/javafx-sdk-21.0.6/lib" \
      --add-modules java.se,jdk.zipfs,javafx.controls,javafx.fxml,javafx.graphics,javafx.web \
      --output custom-runtime

cp javafx-sdk-21.0.6/lib/* custom-runtime/lib/

jpackage \
  --name DistributedStorageSystem \
  --input target/ \
  --main-jar distributed-storage-system-1.0-SNAPSHOT.jar \
  --main-class com.unipi.application.Application \
  --type dmg \
  --dest dist/ \
  --icon ./LogoDSS.icns \
  --runtime-image ./custom-runtime/