package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;


@Data
@Accessors(chain = true)
public class AuthenticationBO  {

    /**
     * 生成签名的时间戳
     */
    private Long timeStamp;

    /**
     * 自定义固定字符串
     */
    private String nonceStr;

    /**
     * 签名
     */
    private String signature;



}
