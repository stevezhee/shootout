/****************** CLIENT CODE ****************/

#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>


unsigned int ffi_connect(void)
{
  int sock;
  struct sockaddr_in serverAddr;
  socklen_t addr_size;

  /*---- Create the socket. The three arguments are: ----*/
  /* 1) Internet domain 2) Stream socket 3) Default protocol (TCP in this case) */
  sock = socket(PF_INET, SOCK_STREAM, 0);

  if(sock < 0)
    {
      exit(-1);
    }
  /*---- Configure settings of the server address struct ----*/
  /* Address family = Internet */
  serverAddr.sin_family = AF_INET;
  /* Set port number, using htons function to use proper byte order */
  serverAddr.sin_port = htons(7891);
  /* Set IP address to localhost */
  serverAddr.sin_addr.s_addr = inet_addr("127.0.0.1");
  /* Set all bits of the padding field to 0 */
  memset(serverAddr.sin_zero, '\0', sizeof serverAddr.sin_zero);  

  /*---- Connect the socket to the server using the address struct ----*/
  addr_size = sizeof serverAddr;
  if(!connect(sock, (struct sockaddr *) &serverAddr, addr_size))
    {
      exit(-1);
    }
  return sock;
}

unsigned int ffi_recv(unsigned int sock, unsigned int count, char *buffer)
{
  ssize_t ret;

  /*---- Read the message from the server into the buffer ----*/
  ret = recv(sock, buffer, count, 0);

  if(ret == 0)
    {
      exit(0); // normal shutdown
    }
  if(ret < 0)
    {
      exit(-1);
    }
  
  return ret;
}

int main(){
  int sock;
  char buffer[1024];

  sock = ffi_connect();

  unsigned int n = ffi_recv(sock, sizeof(buffer), buffer);
  /*---- Print the received message ----*/
  printf("Data received(%d): %s", n, buffer);   

  return 0;
}
