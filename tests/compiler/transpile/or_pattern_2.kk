// Nested
enum Vehicle {
  Car(Color),
  Motor
}

enum Color {
  White,
  Gold,
  Black
}

let isShinyCar = (vehicle: Vehicle): Boolean =>
  switch (vehicle) {
    case Car(White | Gold): true
    case _: false
  }

Car(White).isShinyCar().print()
Car(Gold).isShinyCar().print()
Car(Black).isShinyCar().print()
Motor.isShinyCar().print()