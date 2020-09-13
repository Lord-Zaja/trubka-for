import QtQuick 2.2 as QQ2
import Qt3D.Core 2.0
import Qt3D.Render 2.0
import Qt3D.Input 2.0
import Qt3D.Extras 2
Entity {
    id: sceneRoot

    Camera {
        id: camera
        projectionType: CameraLens.PerspectiveProjection
        fieldOfView: 45
        aspectRatio: 16/9
        nearPlane : 0.1
        farPlane : 1000.0
        position: Qt.vector3d( 0.0, 0.0, -20.0 )
        upVector: Qt.vector3d( 0.0, 1.0, 0.0 )
        viewCenter: Qt.vector3d( 0.0, 0.0, 0.0 )
    }

    components: [external_forward_renderer]

    CylinderMesh {
        id: mesh
        radius: 1
        length: 3
        rings: 100
        slices: 20
    }

    Transform {
        id: transform
        scale: 1.5
        rotation: fromAxisAndAngle(Qt.vector3d(1, 0, 0), 45)
    }

    Material {
        id: material
        effect: Effect {}
    }

    Entity {
        id: mainEntity
        objectName: "mainEntity"
        components: [ mesh, material, transform ]
    }
}
