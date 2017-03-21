# Cargamos los datos, ya descargados en la carpeta, en mnist
from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("MNIST_data/", one_hot=True)

# Importamos la librería de tensorflow
import tensorflow as tf

# Creamos un placeholder, un elemento de TensorFlow
x = tf.placeholder(tf.float32, [None, 784])
# Creamos nuestras variables Tensor, es decir, nuestros pesos y los sesgos
W = tf.Variable(tf.zeros([784, 10]))
b = tf.Variable(tf.zeros([10]))

# Ahora implementamos nuestro modelo.
# matmul será la multiplicación
y = tf.nn.softmax(tf.matmul(x, W) + b)

# Ahora vamos a entrenar nuestro modelo.
# Para ello creamos un nuevo placeholder para la entrada de respuestas correctas:
y_ = tf.placeholder(tf.float32, [None, 10])

# E implementamos la función de entropia cruzada
cross_entropy = tf.reduce_mean(-tf.reduce_sum(y_ * tf.log(y), reduction_indices=[1]))

# Nuestro paso de entrenamiento será
train_step = tf.train.GradientDescentOptimizer(0.5).minimize(cross_entropy)
# Donde minimizamos la entropia cruzada, con un ratio de aprendizaje del 0.5 usando el gradiente descendiente

# Ahora podemos lanzar el modelo en una sesión interactiva
sess = tf.InteractiveSession()
# Inicializamos las variables que hemos creado
tf.global_variables_initializer().run()

# Ahora entrenaremos durante 1000 pasos
for _ in range(1000):
  batch_xs, batch_ys = mnist.train.next_batch(100)
  sess.run(train_step, feed_dict={x: batch_xs, y_: batch_ys})

# Veamos como de bueno es nuestro modelo
correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(y_,1))
# Calculemos el porcentaje de aciertos
accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
# Imprimimos
print(sess.run(accuracy, feed_dict={x: mnist.test.images, y_: mnist.test.labels}))


