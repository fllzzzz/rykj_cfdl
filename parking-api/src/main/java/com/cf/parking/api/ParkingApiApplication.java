package com.cf.parking.api;


import com.alibaba.druid.spring.boot.autoconfigure.DruidDataSourceAutoConfigure;
import com.cf.support.swagger.EnableJdSwagger2;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@EnableJdSwagger2
@ComponentScan(basePackages = {"com.cf"})
@MapperScan("com.cf.parking.dao.mapper")
@EnableTransactionManagement(proxyTargetClass = true)
@EnableAutoConfiguration
@SpringBootApplication(exclude = {DruidDataSourceAutoConfigure.class})
@EnableFeignClients( basePackages = {"com.cf.parking.services.integration"})
public class ParkingApiApplication {

    public static void main(String[] args) {
    	SpringApplication application = new SpringApplication(ParkingApiApplication.class);
    	application.setAllowBeanDefinitionOverriding(true);
    	application.run(args);
       // SpringApplication.run(ParkingApiApplication.class, args);
    }
}
