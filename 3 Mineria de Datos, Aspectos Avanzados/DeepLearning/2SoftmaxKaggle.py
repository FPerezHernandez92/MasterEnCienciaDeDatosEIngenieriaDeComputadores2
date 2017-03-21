#Importación de librerías
import numpy as np
import pandas as pd
import tensorflow as tf

#Parámetros
VALIDATION_SIZE = 2000
BATCH_SIZE = 100

#Convertir la etiqueta de clase a scalares de one-hot
def dense_to_one_hot(labels_dense, num_classes):
	num_labels = labels_dense.shape[0]
	index_offset = np.arange(num_labels) * num_classes
	labels_one_hot = np.zeros((num_labels, num_classes))
	labels_one_hot.flat[index_offset + labels_dense.ravel()] = 1
	return labels_one_hot

def next_batch(batch_size):
	global train_images
	global train_labels
	global index_in_epoch
	global epochs_completed

	start = index_in_epoch
	index_in_epoch += batch_size

	if index_in_epoch > num_examples:
		epochs_completed += 1
		perm = np.arange(num_examples)
		np.random.shuffle(perm)
		train_images = train_images[perm]
		train_labels = train_labels[perm]
		start = 0
		index_in_epoch = batch_size
		assert batch_size <= num_examples
	end = index_in_epoch
	return train_images[start:end], train_labels[start:end]

#Leemos el train
data = pd.read_csv('input/train.csv')
#Extraemos las imágenes y las etiquetas de los datos
images = data.iloc[:,1:].values
images = images.astype(np.float)
images = np.multiply(images, 1.0 / 255.0)
image_size = images.shape[1]
image_width = image_height = np.ceil(np.sqrt(image_size)).astype(np.uint8)
labels_flat = data[[0]].values.ravel()
labels_count = np.unique(labels_flat).shape[0]
labels = dense_to_one_hot(labels_flat, labels_count)
labels = labels.astype(np.uint8)

#Dividir en train y validación
validation_images = images[:VALIDATION_SIZE]
validation_labels = labels[:VALIDATION_SIZE]
train_images = images[VALIDATION_SIZE:]
train_labels = labels[VALIDATION_SIZE:]

# Creamos un placeholder, un elemento de TensorFlow
x = tf.placeholder(tf.float32, [None, image_size]) # image_size = 28*28=784
# Creamos nuestras variables Tensor, es decir, nuestros pesos y los sesgos
W = tf.Variable(tf.zeros([image_size, labels_count])) # labels_count = 10
b = tf.Variable(tf.zeros([labels_count]))

# Ahora implementamos nuestro modelo.
# matmul será la multiplicación
y = tf.nn.softmax(tf.matmul(x, W) + b)

# Ahora vamos a entrenar nuestro modelo.
# Para ello creamos un nuevo placeholder para la entrada de respuestas correctas:
y_ = tf.placeholder(tf.float32, [None, labels_count])

# E implementamos la función de entropia cruzada
cross_entropy = tf.reduce_mean(-tf.reduce_sum(y_ * tf.log(y), reduction_indices=[1]))

# Nuestro paso de entrenamiento será
train_step = tf.train.GradientDescentOptimizer(0.5).minimize(cross_entropy)
# Donde minimizamos la entropia cruzada, con un ratio de aprendizaje del 0.5 usando el gradiente descendiente

# Ahora podemos lanzar el modelo en una sesión interactiva
epochs_completed = 0
index_in_epoch = 0
num_examples = train_images.shape[0]
sess = tf.InteractiveSession()
# Inicializamos las variables que hemos creado
tf.global_variables_initializer().run()

# Ahora entrenaremos durante 1000 pasos
for _ in range(1000):
  batch_xs, batch_ys = next_batch(100)
  sess.run(train_step, feed_dict={x: batch_xs, y_: batch_ys})


# Veamos como de bueno es nuestro modelo
correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(y_,1))
# Calculemos el porcentaje de aciertos
accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
# Imprimimos
validation_accuracy = accuracy.eval(feed_dict={x: validation_images, 
                                                   y_: validation_labels})
print('validation_accuracy => %.4f'%validation_accuracy)

#Realizamos la prueba en el test
test_images = pd.read_csv('input/test.csv').values
test_images = test_images.astype(np.float)
test_images = np.multiply(test_images, 1.0 / 255.0)

predict = tf.argmax(y, 1)

predicted_lables = np.zeros(test_images.shape[0])
for i in range(0,test_images.shape[0]//BATCH_SIZE):
	predicted_lables[i*BATCH_SIZE : (i+1)*BATCH_SIZE] = predict.eval(feed_dict={x: test_images[i*BATCH_SIZE : (i+1)*BATCH_SIZE]})

np.savetxt('result/2SoftmaxKaggle.csv', 
           np.c_[range(1,len(test_images)+1),predicted_lables], 
           delimiter=',', 
           header = 'ImageId,Label', 
           comments = '', 
           fmt='%d')

sess.close()
