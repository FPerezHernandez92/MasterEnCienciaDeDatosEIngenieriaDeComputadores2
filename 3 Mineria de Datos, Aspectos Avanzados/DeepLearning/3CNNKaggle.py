#Importación de librerías
import numpy as np
import pandas as pd
import tensorflow as tf
import time

#Parámetros
VALIDATION_SIZE = 2000
BATCH_SIZE = 100
DISPLAY_STEP = 10
TRAINING_EPOCHS = 20000
DROPOUT_CONV = 0.8
DROPOUT_HIDDEN = 0.6

#Convertir la etiqueta de clase a scalares de one-hot
def dense_to_one_hot(labels_dense, num_classes):
  num_labels = labels_dense.shape[0]
  index_offset = np.arange(num_labels) * num_classes
  labels_one_hot = np.zeros((num_labels, num_classes))
  labels_one_hot.flat[index_offset + labels_dense.ravel()] = 1
  return labels_one_hot

#Función siguiente lote
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

#Creamos las funciones para crear los pesos y bias
def weight_variable(shape):
	initial = tf.truncated_normal(shape, stddev=0.1)
	return tf.Variable(initial)

def bias_variable(shape):
	initial = tf.constant(0.1, shape=shape)
	return tf.Variable(initial)

#Definimos las funciones de convolución y pooling
def conv2d(x, W):
	return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')

def max_pool_2x2(x):
	return tf.nn.max_pool(x, ksize=[1, 2, 2, 1],
							strides=[1, 2, 2, 1], padding='SAME')

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

drop_conv = tf.placeholder(tf.float32)
drop_hidden = tf.placeholder(tf.float32)
# Creamos un placeholder, un elemento de TensorFlow
x = tf.placeholder(tf.float32, [None, image_size])
# Ahora vamos a entrenar nuestro modelo.
# Para ello creamos un nuevo placeholder para la entrada de respuestas correctas:
y_ = tf.placeholder(tf.float32, [None, labels_count])

#Primera capa de convolución. 
W_conv1 = weight_variable([5, 5, 1, 32]) # patch size, input channels, output channels
b_conv1 = bias_variable([32]) # output channels
#Remodelamos la imagen
x_image = tf.reshape(x, [-1,28,28,1]) #width and height image, number of color channels
#Aplicamos la función ReLU y max pool que reducirá la imagen a una 14x14
h_conv1 = tf.nn.relu(conv2d(x_image, W_conv1) + b_conv1)
h_pool1 = max_pool_2x2(h_conv1)

#Segunda capa de convolución
W_conv2 = weight_variable([5,5,32,64])
b_conv2 = bias_variable([64])

h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_conv2)
h_pool2 = max_pool_2x2(h_conv2)

#Ahora que la imagen se ha reducido a 7x7, añadimos una capa conectada con 1024 neuronas
W_fc1 = weight_variable([7 * 7 * 64, 1024])
b_fc1 = bias_variable([1024])
h_pool2_flat = tf.reshape(h_pool2, [-1, 7*7*64])
h_fc1 = tf.nn.relu(tf.matmul(h_pool2_flat, W_fc1) + b_fc1)

#Dropout
h_fc1_drop = tf.nn.dropout(h_fc1, drop_hidden)

#Capa de lectura
W_fc2 = weight_variable([1024, labels_count])
b_fc2 = bias_variable([labels_count])
y_conv = tf.matmul(h_fc1_drop, W_fc2) + b_fc2

cross_entropy = tf.reduce_mean(
            tf.nn.softmax_cross_entropy_with_logits(labels=y_, logits=y_conv))
train_step = tf.train.AdamOptimizer(1e-4).minimize(cross_entropy)
correct_prediction = tf.equal(tf.argmax(y_conv,1), tf.argmax(y_,1))
accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
predict = tf.argmax(y_conv, 1)

epochs_completed = 0
index_in_epoch = 0
num_examples = train_images.shape[0]

sess = tf.InteractiveSession()

sess.run(tf.global_variables_initializer())

for i in range(TRAINING_EPOCHS):
  batch = next_batch(BATCH_SIZE)
  if i%DISPLAY_STEP == 0 or (i+1) == TRAINING_EPOCHS:
    train_accuracy = accuracy.eval(feed_dict={
        x:batch[0], y_: batch[1], drop_conv: DROPOUT_CONV, drop_hidden: DROPOUT_HIDDEN})
    hora_actual = time.strftime("%X")
    print("step %d, training accuracy %.2f, %s"%(i, train_accuracy,hora_actual))
    if i%(DISPLAY_STEP*10) == 0 and i:
            DISPLAY_STEP *= 10
  sess.run(train_step, feed_dict={x: batch[0], y_: batch[1], drop_conv: DROPOUT_CONV, drop_hidden: DROPOUT_HIDDEN})

#Realizamos la prueba en el test
test_images = pd.read_csv('input/test.csv').values
test_images = test_images.astype(np.float)
test_images = np.multiply(test_images, 1.0 / 255.0)

predicted_lables = np.zeros(test_images.shape[0])

for i in range(0,test_images.shape[0]//BATCH_SIZE):
  predicted_lables[i*BATCH_SIZE : (i+1)*BATCH_SIZE] = predict.eval(feed_dict={x: test_images[i*BATCH_SIZE : (i+1)*BATCH_SIZE], drop_conv: 1.0, drop_hidden: 1.0})

np.savetxt('result/3CNNKaggle.csv', 
           np.c_[range(1,len(test_images)+1),predicted_lables], 
           delimiter=',', 
           header = 'ImageId,Label', 
           comments = '', 
           fmt='%d')

sess.close()
