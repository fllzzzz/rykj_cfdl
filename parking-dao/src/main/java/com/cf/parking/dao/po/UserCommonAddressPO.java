package com.cf.parking.dao.po;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;


import java.math.BigDecimal;
import java.util.Date;

/**
 * @author whx
 * @date 2022-11-14 10:06:59
 * @description 用户常用地址表
 */
@Data
@TableName("user_common_address")
@Accessors(chain = true)
public class UserCommonAddressPO {


    /**
     * 常用地址id
     */
    @TableId(value = "user_common_address_id", type = IdType.INPUT)
    private Long userCommonAddressId;

    /**
     * 用户ID
     */
    private Long userId;

    /**
     * 省
     */
    private String province;

    /**
     * 市
     */
    private String city;

    /**
     * 县
     */
    private String adName;

    /**
     * 详细地址
     */
    private String title;

    /**
     * 经度
     */
    private BigDecimal longitude;

    /**
     * 纬度
     */
    private BigDecimal latitude;

    /**
     * 创建时间
     */
    private Date createTm;

    /**
     * 更新时间
     */
    private Date updateTm;
}
